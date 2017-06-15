{-# LANGUAGE BangPatterns, CPP, OverloadedStrings #-}

module Data.Csv.Util
    ( (<$!>)
    , blankLine
    , liftM2'
    , endOfLine
    , doubleQuote
    , newline
    , cr
    , toStrict
    ) where

import Control.Applicative ((<|>))
import Data.Word (Word8)
import Data.Attoparsec.ByteString.Char8 (string)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Data.Attoparsec.ByteString (Parser)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((*>))
#endif

#if MIN_VERSION_bytestring(0,10,0)
import Data.ByteString.Lazy (toStrict)
#else
import qualified Data.ByteString.Lazy as L

toStrict :: L.ByteString -> B.ByteString
toStrict = B.concat . L.toChunks
#endif

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
endOfLine :: Parser ()
endOfLine = (A.word8 newline *> return ()) <|> (string "\r\n" *> return ()) <|> (A.word8 cr *> return ())
{-# INLINE endOfLine #-}

doubleQuote, newline, cr :: Word8
doubleQuote = 34
newline = 10
cr = 13
