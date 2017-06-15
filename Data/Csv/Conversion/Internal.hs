module Data.Csv.Conversion.Internal
    ( decimal
    , realFloat
    ) where

import Data.ByteString.Builder (Builder, toLazyByteString, word8, char8,
                                string8, byteString)
import qualified Data.ByteString.Builder.Prim as BP
import Data.Array.Base (unsafeAt)
import Data.Array.IArray
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.Int
import Data.Monoid
import Data.Word

import Data.Csv.Util (toStrict)

------------------------------------------------------------------------
-- Integers

decimal :: Integral a => a -> B.ByteString
decimal = toStrict . toLazyByteString . formatDecimal
{-# INLINE decimal #-}

-- TODO: Add an optimized version for Integer.

formatDecimal :: Integral a => a -> Builder
{-# RULES "formatDecimal/Int" formatDecimal = formatBoundedSigned
    :: Int -> Builder #-}
{-# RULES "formatDecimal/Int8" formatDecimal = formatBoundedSigned
    :: Int8 -> Builder #-}
{-# RULES "formatDecimal/Int16" formatDecimal = formatBoundedSigned
    :: Int16 -> Builder #-}
{-# RULES "formatDecimal/Int32" formatDecimal = formatBoundedSigned
    :: Int32 -> Builder #-}
{-# RULES "formatDecimal/Int64" formatDecimal = formatBoundedSigned
    :: Int64 -> Builder #-}
{-# RULES "formatDecimal/Word" formatDecimal = formatPositive
    :: Word -> Builder #-}
{-# RULES "formatDecimal/Word8" formatDecimal = formatPositive
    :: Word8 -> Builder #-}
{-# RULES "formatDecimal/Word16" formatDecimal = formatPositive
    :: Word16 -> Builder #-}
{-# RULES "formatDecimal/Word32" formatDecimal = formatPositive
    :: Word32 -> Builder #-}
{-# RULES "formatDecimal/Word64" formatDecimal = formatPositive
    :: Word64 -> Builder #-}
{-# NOINLINE formatDecimal #-}
formatDecimal i
    | i < 0     = minus <>
                  if i <= -128
                  then formatPositive (-(i `quot` 10)) <> digit (-(i `rem` 10))
                  else formatPositive (-i)
    | otherwise = formatPositive i

formatBoundedSigned :: (Integral a, Bounded a) => a -> Builder
{-# SPECIALIZE formatBoundedSigned :: Int -> Builder #-}
{-# SPECIALIZE formatBoundedSigned :: Int8 -> Builder #-}
{-# SPECIALIZE formatBoundedSigned :: Int16 -> Builder #-}
{-# SPECIALIZE formatBoundedSigned :: Int32 -> Builder #-}
{-# SPECIALIZE formatBoundedSigned :: Int64 -> Builder #-}
formatBoundedSigned i
    | i < 0     = minus <>
                  if i == minBound
                  then formatPositive (-(i `quot` 10)) <> digit (-(i `rem` 10))
                  else formatPositive (-i)
    | otherwise = formatPositive i

formatPositive :: Integral a => a -> Builder
{-# SPECIALIZE formatPositive :: Int -> Builder #-}
{-# SPECIALIZE formatPositive :: Int8 -> Builder #-}
{-# SPECIALIZE formatPositive :: Int16 -> Builder #-}
{-# SPECIALIZE formatPositive :: Int32 -> Builder #-}
{-# SPECIALIZE formatPositive :: Int64 -> Builder #-}
{-# SPECIALIZE formatPositive :: Word -> Builder #-}
{-# SPECIALIZE formatPositive :: Word8 -> Builder #-}
{-# SPECIALIZE formatPositive :: Word16 -> Builder #-}
{-# SPECIALIZE formatPositive :: Word32 -> Builder #-}
{-# SPECIALIZE formatPositive :: Word64 -> Builder #-}
formatPositive = go
  where go n | n < 10    = digit n
             | otherwise = go (n `quot` 10) <> digit (n `rem` 10)

minus :: Builder
minus = word8 45

zero :: Word8
zero = 48

digit :: Integral a => a -> Builder
digit n = word8 $! i2w (fromIntegral n)
{-# INLINE digit #-}

i2w :: Int -> Word8
i2w i = zero + fromIntegral i
{-# INLINE i2w #-}

------------------------------------------------------------------------
-- Floating point numbers

realFloat :: RealFloat a => a -> B.ByteString
{-# SPECIALIZE realFloat :: Float -> B.ByteString #-}
{-# SPECIALIZE realFloat :: Double -> B.ByteString #-}
realFloat = toStrict . toLazyByteString . formatRealFloat Generic

-- | Control the rendering of floating point numbers.
data FPFormat = Exponent
              -- ^ Scientific notation (e.g. @2.3e123@).
              | Fixed
              -- ^ Standard decimal notation.
              | Generic
              -- ^ Use decimal notation for values between @0.1@ and
              -- @9,999,999@, and scientific notation otherwise.
                deriving (Enum, Read, Show)

formatRealFloat :: RealFloat a => FPFormat -> a -> Builder
{-# SPECIALIZE formatRealFloat :: FPFormat -> Float -> Builder #-}
{-# SPECIALIZE formatRealFloat :: FPFormat -> Double -> Builder #-}
formatRealFloat fmt x
   | isNaN x                   = string8 "NaN"
   | isInfinite x              = if x < 0
                                 then string8 "-Infinity"
                                 else string8 "Infinity"
   | x < 0 || isNegativeZero x = minus <> doFmt fmt (floatToDigits (-x))
   | otherwise                 = doFmt fmt (floatToDigits x)
 where
  doFmt format (is, e) =
    let ds = map i2d is in
    case format of
     Generic ->
      doFmt (if e < 0 || e > 7 then Exponent else Fixed)
            (is,e)
     Exponent ->
        let show_e' = formatDecimal (e-1) in
        case ds of
          [48]    -> string8 "0.0e0"
          [d]     -> word8 d <> string8 ".0e" <> show_e'
          (d:ds') -> word8 d <> char8 '.' <> word8s ds' <>
                     char8 'e' <> show_e'
          []      -> error "formatRealFloat/doFmt/Exponent: []"
     Fixed
          | e <= 0    -> string8 "0." <>
                         byteString (B.replicate (-e) zero) <>
                         word8s ds
          | otherwise ->
             let
                f 0 s    rs  = mk0 (reverse s) <> char8 '.' <> mk0 rs
                f n s    []  = f (n-1) (zero:s) []
                f n s (r:rs) = f (n-1) (r:s) rs
             in
                f e [] ds
       where mk0 ls = case ls of { [] -> word8 zero ; _ -> word8s ls}

-- Based on "Printing Floating-Point Numbers Quickly and Accurately"
-- by R.G. Burger and R.K. Dybvig in PLDI 96.
-- This version uses a much slower logarithm estimator. It should be improved.

-- | 'floatToDigits' takes a base and a non-negative 'RealFloat' number,
-- and returns a list of digits and an exponent.
-- In particular, if @x>=0@, and
--
-- > floatToDigits base x = ([d1,d2,...,dn], e)
--
-- then
--
--      (1) @n >= 1@
--
--      (2) @x = 0.d1d2...dn * (base**e)@
--
--      (3) @0 <= di <= base-1@

floatToDigits :: (RealFloat a) => a -> ([Int], Int)
{-# SPECIALIZE floatToDigits :: Float -> ([Int], Int) #-}
{-# SPECIALIZE floatToDigits :: Double -> ([Int], Int) #-}
floatToDigits 0 = ([0], 0)
floatToDigits x =
 let
  (f0, e0) = decodeFloat x
  (minExp0, _) = floatRange x
  p = floatDigits x
  b = floatRadix x
  minExp = minExp0 - p -- the real minimum exponent
  -- Haskell requires that f be adjusted so denormalized numbers
  -- will have an impossibly low exponent.  Adjust for this.
  (f, e) =
   let n = minExp - e0 in
   if n > 0 then (f0 `quot` (expt b n), e0+n) else (f0, e0)
  (r, s, mUp, mDn) =
   if e >= 0 then
    let be = expt b e in
    if f == expt b (p-1) then
      (f*be*b*2, 2*b, be*b, be)     -- according to Burger and Dybvig
    else
      (f*be*2, 2, be, be)
   else
    if e > minExp && f == expt b (p-1) then
      (f*b*2, expt b (-e+1)*2, b, 1)
    else
      (f*2, expt b (-e)*2, 1, 1)
  k :: Int
  k =
   let
    k0 :: Int
    k0 =
     if b == 2 then
        -- logBase 10 2 is very slightly larger than 8651/28738
        -- (about 5.3558e-10), so if log x >= 0, the approximation
        -- k1 is too small, hence we add one and need one fixup step less.
        -- If log x < 0, the approximation errs rather on the high side.
        -- That is usually more than compensated for by ignoring the
        -- fractional part of logBase 2 x, but when x is a power of 1/2
        -- or slightly larger and the exponent is a multiple of the
        -- denominator of the rational approximation to logBase 10 2,
        -- k1 is larger than logBase 10 x. If k1 > 1 + logBase 10 x,
        -- we get a leading zero-digit we don't want.
        -- With the approximation 3/10, this happened for
        -- 0.5^1030, 0.5^1040, ..., 0.5^1070 and values close above.
        -- The approximation 8651/28738 guarantees k1 < 1 + logBase 10 x
        -- for IEEE-ish floating point types with exponent fields
        -- <= 17 bits and mantissae of several thousand bits, earlier
        -- convergents to logBase 10 2 would fail for long double.
        -- Using quot instead of div is a little faster and requires
        -- fewer fixup steps for negative lx.
        let lx = p - 1 + e0
            k1 = (lx * 8651) `quot` 28738
        in if lx >= 0 then k1 + 1 else k1
     else
        -- f :: Integer, log :: Float -> Float,
        --               ceiling :: Float -> Int
        ceiling ((log (fromInteger (f+1) :: Float) +
                 fromIntegral e * log (fromInteger b)) /
                   log 10)
--WAS:            fromInt e * log (fromInteger b))

    fixup n =
      if n >= 0 then
        if r + mUp <= expt 10 n * s then n else fixup (n+1)
      else
        if expt 10 (-n) * (r + mUp) <= s then n else fixup (n+1)
   in
   fixup k0

  gen ds rn sN mUpN mDnN =
   let
    (dn, rn') = (rn * 10) `quotRem` sN
    mUpN' = mUpN * 10
    mDnN' = mDnN * 10
   in
   case (rn' < mDnN', rn' + mUpN' > sN) of
    (True,  False) -> dn : ds
    (False, True)  -> dn+1 : ds
    (True,  True)  -> if rn' * 2 < sN then dn : ds else dn+1 : ds
    (False, False) -> gen (dn:ds) rn' sN mUpN' mDnN'

  rds =
   if k >= 0 then
      gen [] r (s * expt 10 k) mUp mDn
   else
     let bk = expt 10 (-k) in
     gen [] (r * bk) s (mUp * bk) (mDn * bk)
 in
 (map fromIntegral (reverse rds), k)

-- Exponentiation with a cache for the most common numbers.
minExpt, maxExpt :: Int
minExpt = 0
maxExpt = 1100

expt :: Integer -> Int -> Integer
expt base n
    | base == 2 && n >= minExpt && n <= maxExpt = expts `unsafeAt` n
    | base == 10 && n <= maxExpt10              = expts10 `unsafeAt` n
    | otherwise                                 = base^n

expts :: Array Int Integer
expts = array (minExpt,maxExpt) [(n,2^n) | n <- [minExpt .. maxExpt]]

maxExpt10 :: Int
maxExpt10 = 324

expts10 :: Array Int Integer
expts10 = array (minExpt,maxExpt10) [(n,10^n) | n <- [minExpt .. maxExpt10]]

-- | Unsafe conversion for decimal digits.
{-# INLINE i2d #-}
i2d :: Int -> Word8
i2d i = fromIntegral (ord '0' + i)

-- | Word8 list rendering
word8s :: [Word8] -> Builder
word8s = BP.primMapListFixed BP.word8
