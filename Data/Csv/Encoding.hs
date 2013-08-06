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
    , encodeByNameSansHeader

    -- ** Encoding and decoding options
    , DecodeOptions(..)
    , defaultDecodeOptions
    , decodeWith
    , decodeByNameWith
    , EncodeOptions(..)
    , defaultEncodeOptions
    , encodeWith
    , encodeByNameWith
    , encodeByNameSansHeaderWith
    ) where

import Blaze.ByteString.Builder (Builder, fromByteString, fromWord8,
                                 toLazyByteString, toByteString)
import Blaze.ByteString.Builder.Char8 (fromString)
import Control.Applicative ((*>), (<|>), optional, pure)
import Data.Attoparsec.Char8 (endOfInput, endOfLine)
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
import Data.Csv.Conversion (FromNamedRecord, FromRecord, ToNamedRecord,
                            ToRecord, parseNamedRecord, parseRecord, runParser,
                            toNamedRecord, toRecord)
import Data.Csv.Parser hiding (csv, csvWithHeader)
import qualified Data.Csv.Parser as Parser
import Data.Csv.Types hiding (toNamedRecord)
import qualified Data.Csv.Types as Types
import Data.Csv.Util (blankLine)

-- TODO: 'encode' isn't as efficient as it could be.

------------------------------------------------------------------------
-- * Encoding and decoding

-- TODO: Change Bool to data HasHeader = HasHeader | NoHeader

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

-- | Efficiently serialize CSV records as a lazy 'L.ByteString'. The
-- header is not written, but does dictate the field order.
encodeByNameSansHeader :: ToNamedRecord a => Header -> V.Vector a -> L.ByteString
encodeByNameSansHeader hdr v = toLazyByteString $
    encodeByNameSansHeaderWith defaultEncodeOptions hdr v
{-# INLINE encodeByNameSansHeader #-}

------------------------------------------------------------------------
-- ** Encoding and decoding options

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a
           => DecodeOptions  -- ^ Decoding options
           -> Bool           -- ^ Data contains header that should be
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
idDecodeWith :: DecodeOptions -> Bool -> L.ByteString
             -> Either String (Vector (Vector B.ByteString))
idDecodeWith = decodeWithC Parser.csv

-- | Decode CSV data using the provided parser, skipping a leading
-- header if 'skipHeader' is 'True'. Returns 'Left' @errMsg@ on
-- failure.
decodeWithC :: (DecodeOptions -> AL.Parser a) -> DecodeOptions -> Bool
            -> BL8.ByteString -> Either String a
decodeWithC p !opts skipHeader = decodeWithP parser
  where parser
            | skipHeader = header (decDelimiter opts) *> p opts
            | otherwise  = p opts
{-# INLINE decodeWithC #-}

-- | Like 'decodeByName', but lets you customize how the CSV data is
-- parsed.
decodeByNameWith :: FromNamedRecord a
                 => DecodeOptions  -- ^ Decoding options
                 -> L.ByteString   -- ^ CSV data
                 -> Either String (Header, Vector a)
decodeByNameWith !opts = decodeWithP (csvWithHeader opts)

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

-- TODO: Optimize
escape :: B.ByteString -> B.ByteString
escape s
    | B.any (\ b -> b == dquote || b == comma || b == nl || b == cr || b == sp)
        s = toByteString $
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
    comma  = 44
    nl     = 10
    cr     = 13
    sp     = 32

-- | Like 'encodeByName', but lets you customize how the CSV data is
-- encoded.
encodeByNameWith :: ToNamedRecord a => EncodeOptions -> Header -> V.Vector a
                 -> L.ByteString
encodeByNameWith opts hdr v =
    toLazyByteString ((encodeRecord (encDelimiter opts) hdr) <>
                      fromByteString "\r\n" <>
                      encodeByNameSansHeaderWith opts hdr v)
{-# INLINE encodeByNameWith #-}

-- | Like 'encodeByNameSansHeader', but lets you customize how the CSV data is
-- encoded.
encodeByNameSansHeaderWith :: ToNamedRecord a => EncodeOptions -> Header
                           -> V.Vector a -> Builder
encodeByNameSansHeaderWith opts hdr v = unlines
              . map (encodeRecord (encDelimiter opts)
                     . namedRecordToRecord hdr . toNamedRecord)
              . V.toList $ v
{-# INLINE encodeByNameSansHeaderWith #-}

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

decodeWithP :: AL.Parser a -> L.ByteString -> Either String a
decodeWithP p s =
    case AL.parse p s of
      AL.Done _ v     -> Right v
      AL.Fail left _ msg -> Left $ "parse error (" ++ msg ++ ") at " ++
                            show (BL8.unpack left)
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
