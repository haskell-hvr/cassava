{-# LANGUAGE BangPatterns, Rank2Types #-}

-- | A RFC 4180 compliant CSV parsing and encodig module.
module Data.Ceason
    (
    -- * Encoding and decoding
      decode
    , encode

    -- * Core CSV types
    , Csv
    , Record
    , Field

    -- * Type conversion
    , Only(..)
    , FromRecord(..)
    , FromField(..)

    -- * Accessors
    , (.!)
    ) where

import Control.Applicative
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Data.Traversable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Prelude hiding (takeWhile)

import Data.Ceason.Parser.Internal
import Data.Ceason.Types

-- | Efficiently deserialize CSV records from a lazy
-- 'L.ByteString'. If this fails due to incomplete or invalid input,
-- 'Nothing' is returned.
decode :: FromRecord a => L.ByteString -> Maybe (Vector a)
decode = decodeWith csv (parse . traverse parseRecord)

-- | Efficiently serialize CVS records as a lazy 'L.ByteString'.
encode :: Csv -> L.ByteString
encode = undefined

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
