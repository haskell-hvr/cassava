{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}

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
      HasHeader(..)
    , decode
    , decodeByName
    , Quoting(..)
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
    ) where

import Blaze.ByteString.Builder (Builder, fromByteString, fromWord8,
                                 toLazyByteString, toByteString)
import Blaze.ByteString.Builder.Char8 (fromString)
import Control.Applicative ((<|>), optional)
import Data.Attoparsec.ByteString.Char8 (endOfInput)
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as HM
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Prelude hiding (unlines)

import Data.Csv.Compat.Monoid ((<>))
import Data.Csv.Conversion (FromNamedRecord, FromRecord, ToNamedRecord,
                            ToRecord, parseNamedRecord, parseRecord, runParser,
                            toNamedRecord, toRecord)
import Data.Csv.Parser hiding (csv, csvWithHeader)
import qualified Data.Csv.Parser as Parser
import Data.Csv.Types hiding (toNamedRecord)
import qualified Data.Csv.Types as Types
import Data.Csv.Util (blankLine, endOfLine)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((*>), pure)
import Data.Monoid (mconcat, mempty)
#endif

-- TODO: 'encode' isn't as efficient as it could be.

------------------------------------------------------------------------
-- * Encoding and decoding

-- | Efficiently deserialize CSV records from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, @'Left' msg@ is
-- returned. Equivalent to @'decodeWith' 'defaultDecodeOptions'@.
decode :: FromRecord a
       => HasHeader     -- ^ Data contains header that should be
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
encode :: ToRecord a => [a] -> L.ByteString
encode = encodeWith defaultEncodeOptions
{-# INLINE encode #-}

-- | Efficiently serialize CSV records as a lazy 'L.ByteString'. The
-- header is written before any records and dictates the field order.
encodeByName :: ToNamedRecord a => Header -> [a] -> L.ByteString
encodeByName = encodeByNameWith defaultEncodeOptions
{-# INLINE encodeByName #-}

------------------------------------------------------------------------
-- ** Encoding and decoding options

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a
           => DecodeOptions  -- ^ Decoding options
           -> HasHeader      -- ^ Data contains header that should be
                             -- skipped
           -> L.ByteString   -- ^ CSV data
           -> Either String (Vector a)
decodeWith = decodeWithC csv
{-# INLINE [1] decodeWith #-}

{-# RULES
    "idDecodeWith" decodeWith = idDecodeWith
 #-}

-- | Same as 'decodeWith', but more efficient as no type
-- conversion is performed.
idDecodeWith :: DecodeOptions -> HasHeader -> L.ByteString
             -> Either String (Vector (Vector B.ByteString))
idDecodeWith = decodeWithC Parser.csv

-- | Decode CSV data using the provided parser, skipping a leading
-- header if 'hasHeader' is 'HasHeader'. Returns 'Left' @errMsg@ on
-- failure.
decodeWithC :: (DecodeOptions -> AL.Parser a) -> DecodeOptions -> HasHeader
            -> BL8.ByteString -> Either String a
decodeWithC p !opts hasHeader = decodeWithP parser
  where parser = case hasHeader of
            HasHeader -> header (decDelimiter opts) *> p opts
            NoHeader  -> p opts
{-# INLINE decodeWithC #-}

-- | Like 'decodeByName', but lets you customize how the CSV data is
-- parsed.
decodeByNameWith :: FromNamedRecord a
                 => DecodeOptions  -- ^ Decoding options
                 -> L.ByteString   -- ^ CSV data
                 -> Either String (Header, Vector a)
decodeByNameWith !opts = decodeWithP (csvWithHeader opts)

-- | Should quoting be applied to text fields, and at which level
data Quoting
    = QuoteNone        -- ^ No quotes
    | QuoteMinimal     -- ^ Quotes if the field has spaces
    | QuoteAll         -- ^ Allways Quote
    deriving (Eq, Show)

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
--
-- /N.B./ The 'encDelimiter' must /not/ be the quote character (i.e.
-- @\"@) or one of the record separator characters (i.e. @\\n@ or
-- @\\r@).
data EncodeOptions = EncodeOptions
    { -- | Field delimiter.
      encDelimiter  :: {-# UNPACK #-} !Word8

      -- | Record separator selection.  @True@ for CRLF (@\\r\\n@) and
      -- @False@ for LF (@\\n@).
    , encUseCrLf :: !Bool

      -- | Include a header row when encoding @ToNamedRecord@
      -- instances.
    , encIncludeHeader :: !Bool

      -- | What kind of quoting should be applied to text fields.
    , encQuoting :: !Quoting
    } deriving (Eq, Show)

-- | Encoding options for CSV files.
defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions
    { encDelimiter     = 44  -- comma
    , encUseCrLf       = True
    , encIncludeHeader = True
    , encQuoting       = QuoteMinimal
    }

-- | Like 'encode', but lets you customize how the CSV data is
-- encoded.
encodeWith :: ToRecord a => EncodeOptions -> [a] -> L.ByteString
encodeWith opts
    | validDelim (encDelimiter opts) =
        toLazyByteString
        . unlines (recordSep (encUseCrLf opts))
        . map (encodeRecord (encQuoting opts) (encDelimiter opts)
              . toRecord)
    | otherwise = encodeOptionsError
{-# INLINE encodeWith #-}

-- | Check if the delimiter is valid.
validDelim :: Word8 -> Bool
validDelim delim = delim `notElem` [cr, nl, dquote]
  where
    nl = 10
    cr = 13
    dquote = 34

-- | Raises an exception indicating that the provided delimiter isn't
-- valid. See 'validDelim'.
--
-- Keep this message consistent with the documentation of
-- 'EncodeOptions'.
encodeOptionsError :: a
encodeOptionsError = error $ "Data.Csv: " ++
        "The 'encDelimiter' must /not/ be the quote character (i.e. " ++
        "\") or one of the record separator characters (i.e. \\n or " ++
        "\\r)"

encodeRecord :: Quoting -> Word8 -> Record -> Builder
encodeRecord qtng delim = mconcat . intersperse (fromWord8 delim)
                     . map fromByteString . map (escape qtng delim) . V.toList
{-# INLINE encodeRecord #-}

-- TODO: Optimize
escape :: Quoting -> Word8 -> B.ByteString -> B.ByteString
escape !qtng !delim !s
    | (qtng == QuoteMinimal &&
        B.any (\ b -> b == dquote || b == delim || b == nl || b == cr || b == sp) s
      ) || qtng == QuoteAll
         = toByteString $
            fromWord8 dquote
            <> B.foldl
                (\ acc b -> acc <> if b == dquote
                    then fromByteString "\"\""
                    else fromWord8 b)
                mempty
                s
            <> fromWord8 dquote
    | otherwise = s
  where
    dquote = 34
    nl     = 10
    cr     = 13
    sp     = 32

-- | Like 'encodeByName', but lets you customize how the CSV data is
-- encoded.
encodeByNameWith :: ToNamedRecord a => EncodeOptions -> Header -> [a]
                 -> L.ByteString
encodeByNameWith opts hdr v
    | validDelim (encDelimiter opts) =
        toLazyByteString (rows (encIncludeHeader opts))
    | otherwise = encodeOptionsError
  where
    rows False = records
    rows True  = encodeRecord (encQuoting opts) (encDelimiter opts) hdr <>
                 recordSep (encUseCrLf opts) <> records
    records = unlines (recordSep (encUseCrLf opts))
              . map (encodeRecord (encQuoting opts) (encDelimiter opts)
                     . namedRecordToRecord hdr . toNamedRecord)
              $ v
{-# INLINE encodeByNameWith #-}


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

recordSep :: Bool -> Builder
recordSep False = fromWord8 10 -- new line (\n)
recordSep True  = fromString "\r\n"

unlines :: Builder -> [Builder] -> Builder
unlines _ [] = mempty
unlines sep (b:bs) = b <> sep <> unlines sep bs

intersperse :: Builder -> [Builder] -> [Builder]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

prependToAll :: Builder -> [Builder] -> [Builder]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep <> x : prependToAll sep xs

decodeWithP :: AL.Parser a -> L.ByteString -> Either String a
decodeWithP p s =
    case AL.parse p s of
      AL.Done _ v     -> Right v
      AL.Fail left _ msg -> Left errMsg
        where
          errMsg = "parse error (" ++ msg ++ ") at " ++
                   (if BL8.length left > 100
                    then (take 100 $ BL8.unpack left) ++ " (truncated)"
                    else show (BL8.unpack left))
{-# INLINE decodeWithP #-}

-- These alternative implementation of the 'csv' and 'csvWithHeader'
-- parsers from the 'Parser' module performs the
-- 'FromRecord'/'FromNamedRecord' conversions on-the-fly, thereby
-- avoiding the need to hold a big 'CSV' value in memory. The 'CSV'
-- type has a quite large memory overhead due to high constant
-- overheads of 'B.ByteString' and 'V.Vector'.

-- TODO: Check that the error messages don't duplicate prefixes, as in
-- "parse error: conversion error: ...".

-- | Parse a CSV file that does not include a header.
csv :: FromRecord a => DecodeOptions -> AL.Parser (V.Vector a)
csv !opts = do
    vals <- records
    _ <- optional endOfLine
    endOfInput
    return $! V.fromList vals
  where
    records = do
        !r <- record (decDelimiter opts)
        if blankLine r
            then (endOfLine *> records) <|> pure []
            else case runParser (parseRecord r) of
                Left msg  -> fail $ "conversion error: " ++ msg
                Right val -> do
                    !vals <- (endOfLine *> records) <|> pure []
                    return (val : vals)
{-# INLINE csv #-}

-- | Parse a CSV file that includes a header.
csvWithHeader :: FromNamedRecord a => DecodeOptions
              -> AL.Parser (Header, V.Vector a)
csvWithHeader !opts = do
    !hdr <- header (decDelimiter opts)
    vals <- records hdr
    _ <- optional endOfLine
    endOfInput
    let !v = V.fromList vals
    return (hdr, v)
  where
    records hdr = do
        !r <- record (decDelimiter opts)
        if blankLine r
            then (endOfLine *> records hdr) <|> pure []
            else case runParser (convert hdr r) of
                Left msg  -> fail $ "conversion error: " ++ msg
                Right val -> do
                    !vals <- (endOfLine *> records hdr) <|> pure []
                    return (val : vals)

    convert hdr = parseNamedRecord . Types.toNamedRecord hdr
