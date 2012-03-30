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
    ) where

import Blaze.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import qualified Data.Vector as V

import Data.Ceason.Types

-- TODO: 'encode' isn't as efficient as it could be.

-- | Efficiently serialize CVS records as a lazy 'L.ByteString'.
encode :: ToRecord a => V.Vector a -> L.ByteString
encode = toLazyByteString
         . mconcat
         . map (fromLazyByteString . L.fromChunks . V.toList . toRecord)
         . V.toList
