{-# LANGUAGE OverloadedStrings #-}

-- Module:      Data.Ceason.Encode
-- Copyright:   (c) 2011 MailRank, Inc.
--              (c) 2012 Johan Tibell
-- License:     BSD3
-- Maintainer:  Johan Tibell <johan.tibell@gmail.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently convert integers and floating point numbers to byte
-- strings.
module Data.Ceason.Encode
    ( encode
    , encodeByName
    , EncodeOptions(..)
    , defaultEncodeOptions
    , encodeWith
    , encodeByNameWith
    ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import qualified Data.Vector as V
import Data.Word (Word8)
import Prelude hiding (unlines)

import Data.Ceason.Types

-- TODO: 'encode' isn't as efficient as it could be.

-- | Options that controls how CSV data is encoded.
data EncodeOptions = EncodeOptions
    { -- | Field delimiter.
      encDelimiter  :: {-# UNPACK #-} !Word8
    }

-- | Default encoding options:
--
--  * 'encDelimiter': comma
defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions
    { encDelimiter = 44  -- comma
    }

-- | Efficiently serialize CVS records as a lazy 'L.ByteString'.
encode :: ToRecord a => V.Vector a -> L.ByteString
encode = encodeWith defaultEncodeOptions

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

-- | Efficiently serialize CVS records as a lazy 'L.ByteString'. The
-- header is written before any records and dictates the field order.
encodeByName :: ToNamedRecord a => Header -> V.Vector a -> L.ByteString
encodeByName = encodeByNameWith defaultEncodeOptions

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
                   "header contains name '" ++ B8.unpack n ++
                   "' which is not present in the named record"
        Just v  -> v

moduleError :: String -> String -> a
moduleError func msg = error $ "Data.Ceason.Encode." ++ func ++ ": " ++ msg
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
