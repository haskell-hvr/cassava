{-# LANGUAGE MagicHash #-}
module Data.Ceason.Encode
    ( decimal
    ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import qualified Data.ByteString as B
import Data.Int
import Data.Monoid
import Data.Word
import GHC.Exts

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
    | i < 0     = fromChar '-' <> go (-i)
    | otherwise = go i
  where
    go n | n < 10    = digit n
         | otherwise = go (n `quot` 10) <> digit (n `rem` 10)

digit :: Integral a => a -> Builder
digit n = fromChar $! i2d (fromIntegral n)
{-# INLINE digit #-}

i2d :: Int -> Char
i2d (I# i#) = C# (chr# (ord# '0'# +# i#))
{-# INLINE i2d #-}
