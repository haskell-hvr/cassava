{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Data.Csv.Util
    ( (<$!>)
    , blankLine
    , liftM2'
    , endOfLine
    , doubleQuote
    , newline
    , cr
    ) where

import Control.Applicative ((<|>), (*>))
import Data.Word (Word8)
import Data.Attoparsec.ByteString.Char8 (string)
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Attoparsec.Types (Parser)

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

-- | A version of 'liftM2' that is strict in the result of its first
-- action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
    !x <- a
    y <- b
    return (f x y)
{-# INLINE liftM2' #-}


-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@, or a single
-- carriage return @\'\\r\'@.
endOfLine :: Parser B.ByteString ()
endOfLine = (A.word8 newline *> return ()) <|> (string "\r\n" *> return ()) <|> (A.word8 cr *> return ())
{-# INLINE endOfLine #-}

doubleQuote, newline, cr :: Word8
doubleQuote = 34
newline = 10
cr = 13
