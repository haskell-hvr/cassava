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
    ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Control.Applicative
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Traversable
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import Prelude hiding (unlines)

import Data.Csv.Conversion
import Data.Csv.Parser
import Data.Csv.Types

-- TODO: 'encode' isn't as efficient as it could be.

------------------------------------------------------------------------
-- * Encoding and decoding

-- | Efficiently deserialize CSV records from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, @'Left' msg@ is
-- returned. Equivalent to @'decodeWith' 'defaultDecodeOptions'@.
--
-- Use the 'decSkipHeader' and 'decodeWith' if the input has a header.
decode :: FromRecord a => L.ByteString -> Either String (Vector a)
decode = decodeWith defaultDecodeOptions
{-# INLINE decode #-}

-- | Efficiently deserialize CSV records from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, @'Left' msg@ is
-- returned. The data is assumed to be preceeded by a header.
-- Equivalent to @'decodeByNameWith' 'defaultDecodeOptions'@.
decodeByName :: FromNamedRecord a => L.ByteString
                 -> Either String (Header, Vector a)
decodeByName = decodeByNameWith defaultDecodeOptions
{-# INLINE decodeByName #-}

-- | Efficiently serialize CVS records as a lazy 'L.ByteString'.
encode :: ToRecord a => V.Vector a -> L.ByteString
encode = encodeWith defaultEncodeOptions

-- | Efficiently serialize CVS records as a lazy 'L.ByteString'. The
-- header is written before any records and dictates the field order.
encodeByName :: ToNamedRecord a => Header -> V.Vector a -> L.ByteString
encodeByName = encodeByNameWith defaultEncodeOptions

------------------------------------------------------------------------
-- ** Encoding and decoding options

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a => DecodeOptions -> L.ByteString
           -> Either String (Vector a)
decodeWith !opts = decodeWithP (csv opts) (parse . traverse parseRecord)
{-# INLINE [1] decodeWith #-}

{-# RULES
    "idDecodeWith" decodeWith = idDecodeWith
 #-}

-- | Same as 'decodeWith', but more efficient as no type
-- conversion is performed.
idDecodeWith :: DecodeOptions -> L.ByteString
             -> Either String (Vector (Vector B.ByteString))
idDecodeWith !opts = decodeWithP (csv opts) pure

-- | Like 'decodeByName', but lets you customize how the CSV data is
-- parsed.
decodeByNameWith :: FromNamedRecord a => DecodeOptions -> L.ByteString
                     -> Either String (Header, Vector a)
decodeByNameWith !opts =
    decodeWithP (csvWithHeader opts)
    (\ (hdr, vs) -> (,) <$> pure hdr <*> (parse $ traverse parseNamedRecord vs))

-- | Options that controls how data is encoded. These options can be
-- used to e.g. encode data in a tab-separated format instead of in a
-- comma-separated format.
data EncodeOptions = EncodeOptions
    { -- | Field delimiter.
      encDelimiter  :: {-# UNPACK #-} !Word8
    }

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

encodeRecord :: Word8 -> Record -> Builder
encodeRecord delim = mconcat . intersperse (fromWord8 delim)
                     . map fromByteString . V.toList
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

decodeWithP :: AL.Parser a -> (a -> Result b) -> L.ByteString -> Either String b
decodeWithP p to s =
    case AL.parse p s of
      AL.Done _ v     -> case to v of
          Success a -> Right a
          Error msg -> Left $ "conversion error: " ++ msg
      AL.Fail left _ msg -> Left $ "parse error (" ++ msg ++ ") at " ++
                            show (BL8.unpack left)
{-# INLINE decodeWithP #-}