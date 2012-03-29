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
    ( decimal
    ) where

import Blaze.ByteString.Builder
import qualified Data.ByteString as B
import Data.Int
import Data.Monoid
import Data.Word

decimal :: Integral a => a -> B.ByteString
{-# SPECIALIZE decimal :: Int -> B.ByteString #-}
{-# SPECIALIZE decimal :: Int8 -> B.ByteString #-}
{-# SPECIALIZE decimal :: Int16 -> B.ByteString #-}
{-# SPECIALIZE decimal :: Int32 -> B.ByteString #-}
{-# SPECIALIZE decimal :: Int64 -> B.ByteString #-}
{-# SPECIALIZE decimal :: Word -> B.ByteString #-}
{-# SPECIALIZE decimal :: Word8 -> B.ByteString #-}
{-# SPECIALIZE decimal :: Word16 -> B.ByteString #-}
{-# SPECIALIZE decimal :: Word32 -> B.ByteString #-}
{-# SPECIALIZE decimal :: Word64 -> B.ByteString #-}
decimal = toByteString . decimal_

decimal_ :: Integral a => a -> Builder
{-# SPECIALIZE decimal_ :: Int -> Builder #-}
{-# SPECIALIZE decimal_ :: Int8 -> Builder #-}
{-# SPECIALIZE decimal_ :: Int16 -> Builder #-}
{-# SPECIALIZE decimal_ :: Int32 -> Builder #-}
{-# SPECIALIZE decimal_ :: Int64 -> Builder #-}
{-# SPECIALIZE decimal_ :: Word -> Builder #-}
{-# SPECIALIZE decimal_ :: Word8 -> Builder #-}
{-# SPECIALIZE decimal_ :: Word16 -> Builder #-}
{-# SPECIALIZE decimal_ :: Word32 -> Builder #-}
{-# SPECIALIZE decimal_ :: Word64 -> Builder #-}
decimal_ i
    | i < 0     = fromWord8 minus <> go (-i)
    | otherwise = go i
  where
    go n | n < 10    = digit n
         | otherwise = go q <> digit r
      where (q, r) = n `quotRem` 10

minus, zero :: Word8
minus = 45
zero = 48

digit :: Integral a => a -> Builder
digit n = fromWord8 $! i2w (fromIntegral n)
{-# INLINE digit #-}

i2w :: Int -> Word8
i2w i = zero + fromIntegral i
{-# INLINE i2w #-}
