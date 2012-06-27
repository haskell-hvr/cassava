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
import Blaze.ByteString.Builder.Char8
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import qualified Data.Vector as V
import Prelude hiding (unlines)

import Data.Ceason.Types

-- TODO: 'encode' isn't as efficient as it could be.

-- | Efficiently serialize CVS records as a lazy 'L.ByteString'.
encode :: ToRecord a => V.Vector a -> L.ByteString
encode = toLazyByteString
         . unlines
         . map (mconcat . intersperse (fromChar ',') . map fromByteString
                . V.toList . toRecord)
         . V.toList

-- TODO: Implement
encodeWithHeader :: ToNamedRecord a => V.Vector a -> L.ByteString
encodeWithHeader = undefined

unlines :: [Builder] -> Builder
unlines [] = mempty
unlines (b:bs) = b <> fromString "\r\n" <> unlines bs

intersperse :: Builder -> [Builder] -> [Builder]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs

prependToAll :: Builder -> [Builder] -> [Builder]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep <> x : prependToAll sep xs
