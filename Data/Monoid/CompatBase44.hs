-- | This is a compatibility hack for base < 4.5

module Data.Monoid.CompatBase44 where

import Data.Monoid

infixr 6 <>
-- | An infix synonym for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

