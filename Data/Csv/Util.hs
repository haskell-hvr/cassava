module Data.Csv.Util
      ((<$!>)
    , blankLine
    ) where

import qualified Data.ByteString as B
import qualified Data.Vector as V

-- | A strict version of 'Data.Functor.<$>' for monads.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> m = do
    a <- m
    return $! f a
{-# INLINE (<$!>) #-}

infixl 4 <$!>

-- | Is this an empty record (i.e. a blank line)?
blankLine :: V.Vector B.ByteString -> Bool
blankLine v = V.length v == 1 && (B.null (V.head v))
