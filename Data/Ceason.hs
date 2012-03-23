{-# LANGUAGE BangPatterns, Rank2Types #-}

-- | A RFC 4180 compliant CSV parsing and encodig module.
module Data.Ceason
    (
    -- * Encoding and decoding
      decode
    , encode

    -- * Core CSV types
    , Record
    , Field

    -- * Type conversion
    , Only(..)
    , FromRecord(..)
    , FromField(..)

    -- * Accessors
    , (.!)
    ) where

import Blaze.ByteString.Builder (fromByteString, toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import Control.Applicative
import Data.Attoparsec.Char8 hiding (Parser, Result, parse)
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Lazy as L
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as S
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Data.Traversable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Word
import Prelude hiding (takeWhile)

import Data.Ceason.Types

-- | Efficiently deserialize CSV records from a lazy
-- 'L.ByteString'. If this fails due to incomplete or invalid input,
-- 'Nothing' is returned.
decode :: FromRecord a => L.ByteString -> Maybe (Vector a)
decode s =
    case L.parse csv s of
        L.Done _ v -> case parse (traverse parseRecord v) of
            Success a -> Just a
            _         -> Nothing
        _          -> Nothing

-- | Efficiently serialize CVS records as a lazy 'L.ByteString'.
encode :: Vector Record -> L.ByteString
encode = undefined

csv :: L.Parser (Vector Record)
csv = do
    vals <- record `sepBy` endOfLine
    _ <- optional endOfLine
    endOfInput
    return (V.fromList (filterLastIfEmpty vals))
  where
    filterLastIfEmpty [] = []
    filterLastIfEmpty [v]
        | V.length v == 1 && (S.null (V.head v)) = []
    filterLastIfEmpty (v:vs) = v : filterLastIfEmpty vs

record :: L.Parser Record
record = V.fromList <$> field `sepBy` comma

field :: L.Parser Field
field = escapedField <|> unescapedField

escapedField :: L.Parser S.ByteString
escapedField = do
    _ <- dquote
    -- The scan state is 'True' if the previous character was a double
    -- quote.  We need to drop a trailing double quote left by scan.
    s <- S.init <$> (A.scan False $ \s c -> if c == doubleQuote
                                            then if s then Just False
                                                 else Just True
                                            else Just False)
    if doubleQuote `S.elem` s
        then case Z.parse unescape s of
            Right r  -> return r
            Left err -> fail err
        else return s

-- TODO: Perhaps allow all possible bytes (except , and ").
unescapedField :: L.Parser S.ByteString
unescapedField = A.takeWhile isTextdata
  where
    isTextdata :: Word8 -> Bool
    isTextdata c = c >= 0x20 && c <= 0x21 ||
                   c >= 0x23 && c <= 0x2b ||
                   c >= 0x2d && c <= 0x7e

comma, dquote :: L.Parser Char
comma = char ','
dquote = char '"'

unescape :: Z.Parser S.ByteString
unescape = toByteString <$> go mempty where
  go acc = do
    h <- Z.takeWhile (/= doubleQuote)
    let rest = do
          start <- Z.take 2
          if (S.unsafeHead start == doubleQuote &&
              S.unsafeIndex start 1 == doubleQuote)
              then go (acc `mappend` fromByteString h `mappend` fromChar '"')
              else fail "invalid CSV escape sequence"
    done <- Z.atEnd
    if done
      then return (acc `mappend` fromByteString h)
      else rest

doubleQuote :: Word8
doubleQuote = 34

-- | A record corresponds to a single line in a CSV file.
type Record = Vector Field

-- | A single field within a record.
type Field = S.ByteString

-- | A type that can be converted from a single CSV record, with the
-- possibility of failure.
--
-- When writing an instance, use 'empty', 'mzero', or 'fail' to make a
-- conversion fail, e.g. if a 'Record' has the wrong number of
-- columns.
class FromRecord a where
    parseRecord :: Record -> Parser a

-- | Haskell lacks a single-element tuple type, so if you CSV data
-- with just one column you can use the 'Only' type to represent a
-- single-column result.
newtype Only a = Only {
      fromOnly :: a
    } deriving (Eq, Ord, Read, Show)

instance FromField a => FromRecord (Only a) where
    parseRecord v
        | n == 1    = Only <$> parseField (V.unsafeIndex v 0)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 'Only'"
          where
            n = V.length v

instance (FromField a, FromField b) => FromRecord (a, b) where
    parseRecord v
        | n == 2    = (,) <$> parseField (V.unsafeIndex v 0)
                          <*> parseField (V.unsafeIndex v 1)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a pair"
          where
            n = V.length v

instance (FromField a, FromField b, FromField c) => FromRecord (a, b, c) where
    parseRecord v
        | n == 3    = (,,) <$> parseField (V.unsafeIndex v 0)
                           <*> parseField (V.unsafeIndex v 1)
                           <*> parseField (V.unsafeIndex v 2)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 3-tuple"
          where
            n = V.length v

instance (FromField a, FromField b, FromField c, FromField d) =>
         FromRecord (a, b, c, d) where
    parseRecord v
        | n == 4    = (,,,) <$> parseField (V.unsafeIndex v 0)
                            <*> parseField (V.unsafeIndex v 1)
                            <*> parseField (V.unsafeIndex v 2)
                            <*> parseField (V.unsafeIndex v 3)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 4-tuple"
          where
            n = V.length v

instance (FromField a, FromField b, FromField c, FromField d, FromField e) =>
         FromRecord (a, b, c, d, e) where
    parseRecord v
        | n == 5    = (,,,,) <$> parseField (V.unsafeIndex v 0)
                             <*> parseField (V.unsafeIndex v 1)
                             <*> parseField (V.unsafeIndex v 2)
                             <*> parseField (V.unsafeIndex v 3)
                             <*> parseField (V.unsafeIndex v 4)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 5-tuple"
          where
            n = V.length v

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f) =>
         FromRecord (a, b, c, d, e, f) where
    parseRecord v
        | n == 6    = (,,,,,) <$> parseField (V.unsafeIndex v 0)
                              <*> parseField (V.unsafeIndex v 1)
                              <*> parseField (V.unsafeIndex v 2)
                              <*> parseField (V.unsafeIndex v 3)
                              <*> parseField (V.unsafeIndex v 4)
                              <*> parseField (V.unsafeIndex v 5)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 6-tuple"
          where
            n = V.length v

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g) =>
         FromRecord (a, b, c, d, e, f, g) where
    parseRecord v
        | n == 7    = (,,,,,,) <$> parseField (V.unsafeIndex v 0)
                               <*> parseField (V.unsafeIndex v 1)
                               <*> parseField (V.unsafeIndex v 2)
                               <*> parseField (V.unsafeIndex v 3)
                               <*> parseField (V.unsafeIndex v 4)
                               <*> parseField (V.unsafeIndex v 5)
                               <*> parseField (V.unsafeIndex v 6)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 7-tuple"
          where
            n = V.length v

-- | A type that can be converted from a single CSV field, with the
-- possibility of failure.
--
-- When writing an instance, use 'empty', 'mzero', or 'fail' to make a
-- conversion fail, e.g. if a 'Field' has the wrong type.
class FromField a where
    parseField :: Field -> Parser a

instance FromField S.ByteString where
    parseField = pure

instance FromField L.ByteString where
    parseField s = pure (L.fromChunks [s])

instance FromField T.Text where
    parseField = pure . T.decodeUtf8

instance FromField LT.Text where
    parseField s = pure (LT.fromChunks [T.decodeUtf8 s])

-- | Retrieve the /i/th field in the given record.  The result is
-- 'empty' if the value cannot be converted to the desired type.
(.!) :: FromField a => Record -> Int -> Parser a
v .! idx = parseField (v ! idx)
{-# INLINE (.!) #-}
