{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- Module:      Data.Csv.Encoding
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2012 Johan Tibell
-- License:     BSD3
-- Maintainer:  Johan Tibell <johan.tibell@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Encoding and decoding of data types into CSV.
module Data.Csv.Encoding
    (     
    -- * Encoding and decoding
      decode
    , decodeByName
    , encode
    , encodeByName

    -- ** Encoding and decoding options
    , DecodeOptions(..)
    , defaultDecodeOptions
    , decodeWith
    , decodeByNameWith
    , EncodeOptions(..)
    , defaultEncodeOptions
    , encodeWith
    , encodeByNameWith

    -- * Space-delimited files
    , decodeTable
    , decodeTableByName
    , encodeTable
    , encodeTableByName
    ) where

import Blaze.ByteString.Builder (Builder, fromByteString, fromWord8,
                                 toLazyByteString)
import Blaze.ByteString.Builder.Char8 (fromString)
import Control.Applicative ((*>), (<$>), (<*>), pure)
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as HM
import Data.Monoid (mconcat, mempty)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Prelude hiding (unlines)

import Data.Csv.Compat.Monoid ((<>))
import Data.Csv.Conversion
import Data.Csv.Parser
import Data.Csv.Types
import Data.Csv.Util ((<$!>))

-- TODO: 'encode' isn't as efficient as it could be.

------------------------------------------------------------------------
-- * Encoding and decoding

-- | Efficiently deserialize CSV records from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, @'Left' msg@ is
-- returned. Equivalent to @'decodeWith' 'defaultDecodeOptions'@.
decode :: FromRecord a
       => Bool          -- ^ Data contains header that should be
                        -- skipped
       -> L.ByteString  -- ^ CSV data
       -> Either String (Vector a)
decode = decodeWith defaultDecodeOptions
{-# INLINE decode #-}

-- | Efficiently deserialize CSV records from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, @'Left' msg@ is
-- returned. The data is assumed to be preceeded by a header.
-- Equivalent to @'decodeByNameWith' 'defaultDecodeOptions'@.
decodeByName :: FromNamedRecord a
             => L.ByteString  -- ^ CSV data
             -> Either String (Header, Vector a)
decodeByName = decodeByNameWith defaultDecodeOptions
{-# INLINE decodeByName #-}

-- | Efficiently serialize CSV records as a lazy 'L.ByteString'.
encode :: ToRecord a => V.Vector a -> L.ByteString
encode = encodeWith defaultEncodeOptions
{-# INLINE encode #-}

-- | Efficiently serialize CSV records as a lazy 'L.ByteString'. The
-- header is written before any records and dictates the field order.
encodeByName :: ToNamedRecord a => Header -> V.Vector a -> L.ByteString
encodeByName = encodeByNameWith defaultEncodeOptions
{-# INLINE encodeByName #-}

------------------------------------------------------------------------
-- ** Encoding and decoding options

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a
           => DecodeOptions  -- ^ Decoding options
           -> Bool           -- ^ Data contains header that should be
                             -- skipped
           -> L.ByteString   -- ^ CSV data
           -> Either String (Vector a)
decodeWith !opt =
  decodeWithC (header $ decDelimiter opt) (csv opt) (runParser . parseCsv)
{-# INLINE [1] decodeWith #-}

{-# RULES
    "idDecodeWith" decodeWith = idDecodeWith
 #-}

-- | Same as 'decodeWith', but more efficient as no type
-- conversion is performed.
idDecodeWith :: DecodeOptions -> Bool -> L.ByteString
             -> Either String (Vector (Vector B.ByteString))
idDecodeWith !opt = decodeWithC (header $ decDelimiter opt) (csv opt) pure

decodeWithC :: AL.Parser Header -> AL.Parser Csv
            -> (Csv -> Either String a)
            -> Bool -> L.ByteString -> Either String a
decodeWithC headerP body convert skipHeader
  = decodeWithP parser convert
  where parser
            | skipHeader = headerP *> body
            | otherwise  = body
{-# INLINE decodeWithC #-}

-- | Like 'decodeByName', but lets you customize how the CSV data is
-- parsed.
decodeByNameWith :: FromNamedRecord a
                 => DecodeOptions  -- ^ Decoding options
                 -> L.ByteString   -- ^ CSV data
                 -> Either String (Header, Vector a)
decodeByNameWith !opts =
    decodeWithP (csvWithHeader opts)
    (\ (hdr, vs) -> (,) <$> pure hdr <*> (runParser $ parseNamedCsv vs))

-- | Options that controls how data is encoded. These options can be
-- used to e.g. encode data in a tab-separated format instead of in a
-- comma-separated format.
--
-- To avoid having your program stop compiling when new fields are
-- added to 'EncodeOptions', create option records by overriding
-- values in 'defaultEncodeOptions'. Example:
--
-- > myOptions = defaultEncodeOptions {
-- >       encDelimiter = fromIntegral (ord '\t')
-- >     }
data EncodeOptions = EncodeOptions
    { -- | Field delimiter.
      encDelimiter  :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

-- | Encoding options for CSV files.
defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions
    { encDelimiter = 44  -- comma
    }

-- | Like 'encode', but lets you customize how the CSV data is
-- encoded.
encodeWith :: ToRecord a => EncodeOptions -> V.Vector a -> L.ByteString
encodeWith opts = toLazyByteString
                  . unlines
                  . map (encodeRecord (encDelimiter opts) . toRecord)
                  . V.toList
{-# INLINE encodeWith #-}

encodeRecord :: Word8 -> Record -> Builder
encodeRecord delim = mconcat . intersperse (fromWord8 delim)
                     . map fromByteString . map escape . V.toList
{-# INLINE encodeRecord #-}

-- | Like 'encodeByName', but lets you customize how the CSV data is
-- encoded.
encodeByNameWith :: ToNamedRecord a => EncodeOptions -> Header -> V.Vector a
                 -> L.ByteString
encodeByNameWith opts hdr v =
    toLazyByteString ((encodeRecord (encDelimiter opts) hdr) <>
                      fromByteString "\r\n" <> records)
  where
    records = unlines
              . map (encodeRecord (encDelimiter opts)
                     . namedRecordToRecord hdr . toNamedRecord)
              . V.toList $ v
{-# INLINE encodeByNameWith #-}


------------------------------------------------------------------------
-- * Space-delimited files

-- | Efficiently deserialize space-delimited records from a lazy
-- ByteString. If this fails due to incomplete or invalid input,
-- @'Left' msg@ is returned.
decodeTable :: FromRecord a
            => Bool             -- ^ Data contains header that should be skipped
            -> L.ByteString     -- ^ Raw data
            -> Either String (Vector a)
decodeTable =
    decodeWithC tableHeader table (runParser . parseCsv)
{-# INLINE decodeTable #-}

-- | Same as 'decodeWith', but more efficient as no type
-- conversion is performed.
idDecodeTable :: Bool -> L.ByteString -> Either String (Vector (Vector B.ByteString))
idDecodeTable = decodeWithC tableHeader table pure

{-# RULES
    "idDecodeTable" decodeTable = idDecodeTable
 #-}


-- | Efficiently deserialize space-delimited records from a lazy
-- ByteString. If this fails due to incomplete or invalid input,
-- @'Left' msg@ is returned. The data is assumed to be preceeded by a
-- header.
decodeTableByName :: FromNamedRecord a => L.ByteString -> Either String (Header, Vector a)
decodeTableByName =
    decodeWithP tableWithHeader
    (\(hdr, vs) -> (,) <$> pure hdr <*> (runParser $ parseNamedCsv vs))
{-# INLINE decodeTableByName #-}

-- | Efficiently serialize space-delimited records as a lazy
-- ByteString. Single tab is used as separator.
encodeTable :: ToRecord a => V.Vector a -> L.ByteString
encodeTable = toLazyByteString
            . unlines
            . map (encodeTableRow . toRecord)
            . V.toList
{-# INLINE encodeTable #-}

-- | Efficiently serialize space-delimited records as a lazy
-- ByteString. The header is written before any records and dictates
-- the field order. Single tab is used as separator.
encodeTableByName :: ToNamedRecord a => Header -> V.Vector a -> L.ByteString
encodeTableByName hdr v =
    toLazyByteString (  encodeTableRow hdr
                     <> fromByteString "\r\n"
                     <> records )
  where
    records = unlines
            . map (encodeTableRow . namedRecordToRecord hdr . toNamedRecord)
            . V.toList $ v
{-# INLINE encodeTableByName #-}

encodeTableRow :: Record -> Builder
encodeTableRow = mconcat . intersperse (fromWord8 9)
               . map fromByteString . map escapeT . V.toList
  where
    -- We need to escape empty strings. Otherwise we'll get:
    --  > encode ["a","","b"] = 'a  b'
    -- instead of
    --  > encode ["a","","b"] = 'a "" b'
    escapeT b | B.null b  = "\"\""
              | otherwise = escape b
{-# INLINE encodeTableRow #-}


------------------------------------------------------------------------
-- * Common functionality and helpers

parseCsv :: FromRecord a => Csv -> Parser (Vector a)
parseCsv xs = V.fromList <$!> mapM' parseRecord (V.toList xs)

parseNamedCsv :: FromNamedRecord a => Vector NamedRecord -> Parser (Vector a)
parseNamedCsv xs = V.fromList <$!> mapM' parseNamedRecord (V.toList xs)

-- TODO: Optimize
escape :: B.ByteString -> B.ByteString
escape s
    | B.find (\ b -> b == dquote || b == comma || b == nl || b == cr ||
                     b == sp     || b == tab) s == Nothing = s
    | otherwise =
        B.concat ["\"",
                  B.concatMap
                  (\ b -> if b == dquote then "\"\"" else B.singleton b) s,
                  "\""]
  where
    dquote = 34
    comma  = 44
    nl     = 10
    cr     = 13
    sp     = 32
    tab    =  9

namedRecordToRecord :: Header -> NamedRecord -> Record
namedRecordToRecord hdr nr = V.map find hdr
  where
    find n = case HM.lookup n nr of
        Nothing -> moduleError "namedRecordToRecord" $
                   "header contains name " ++ show (B8.unpack n) ++
                   " which is not present in the named record"
        Just v  -> v

moduleError :: String -> String -> a
moduleError func msg = error $ "Data.Csv.Encoding." ++ func ++ ": " ++ msg
{-# NOINLINE moduleError #-}

unlines :: [Builder] -> Builder
unlines [] = mempty
unlines (b:bs) = b <> fromString "\r\n" <> unlines bs

intersperse :: Builder -> [Builder] -> [Builder]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

prependToAll :: Builder -> [Builder] -> [Builder]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep <> x : prependToAll sep xs

decodeWithP :: AL.Parser a -> (a -> Either String b) -> L.ByteString -> Either String b
decodeWithP p to s =
    case AL.parse p s of
      AL.Done _ v     -> case to v of
          Right a  -> Right a
          Left msg -> Left $ "conversion error: " ++ msg
      AL.Fail left _ msg -> Left $ "parse error (" ++ msg ++ ") at " ++
                            show (BL8.unpack left)
{-# INLINE decodeWithP #-}

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = go
  where
    go [] = return []
    go (x:xs) = do
        !y <- f x
        ys <- go xs
        return (y : ys)
{-# INLINE mapM' #-}

