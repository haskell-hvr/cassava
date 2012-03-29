{-# LANGUAGE BangPatterns, Rank2Types #-}

-- | RFC 4180 compliant CSV parsing and encodig.
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
    , ToRecord(..)
    , ToField(..)

    -- * Accessors
    , (.!)
    ) where

import Control.Applicative
import Data.Attoparsec.Char8 (double, number, parseOnly)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import Data.Traversable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Word
import GHC.Float (double2Float)
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

------------------------------------------------------------------------
-- Type conversion

-- | A type that can be converted from a single CSV record, with the
-- possibility of failure.
--
-- When writing an instance, use 'empty', 'mzero', or 'fail' to make a
-- conversion fail, e.g. if a 'Record' has the wrong number of
-- columns.
--
-- Given this example data:
--
-- > John,56
-- > Jane,55
--
-- here's an example type and instance:
--
-- @data Person = Person { name :: Text, age :: Int }
--
-- instance FromRecord Person where
--     parseRecord v
--         | 'V.length' v == 2 = Person '<$>'
--                           v '.!' 0 '<*>'
--                           v '.!' 1
--         | otherwise     = mzero
-- @
class FromRecord a where
    parseRecord :: Record -> Parser a

-- | Haskell lacks a single-element tuple type, so if you CSV data
-- with just one column you can use the 'Only' type to represent a
-- single-column result.
newtype Only a = Only {
      fromOnly :: a
    } deriving (Eq, Ord, Read, Show)

class ToRecord a where
    toRecord :: a -> Record

class ToField a where
    toField :: a -> Field

instance FromField a => FromRecord (Only a) where
    parseRecord v
        | n == 1    = Only <$> parseField (V.unsafeIndex v 0)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 'Only'"
          where
            n = V.length v

-- TODO: Check if we want all toRecord conversions to be stricter.

instance ToField a => ToRecord (Only a) where
    toRecord = V.singleton . toField . fromOnly

instance (FromField a, FromField b) => FromRecord (a, b) where
    parseRecord v
        | n == 2    = (,) <$> parseField (V.unsafeIndex v 0)
                          <*> parseField (V.unsafeIndex v 1)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a pair"
          where
            n = V.length v

instance (ToField a, ToField b) => ToRecord (a, b) where
    toRecord (a, b) = V.fromList [toField a, toField b]

instance (FromField a, FromField b, FromField c) => FromRecord (a, b, c) where
    parseRecord v
        | n == 3    = (,,) <$> parseField (V.unsafeIndex v 0)
                           <*> parseField (V.unsafeIndex v 1)
                           <*> parseField (V.unsafeIndex v 2)
        | otherwise = fail $ "cannot unpack array of length " ++
                        show n ++ " into a 3-tuple"
          where
            n = V.length v

instance (ToField a, ToField b, ToField c) =>
         ToRecord (a, b, c) where
    toRecord (a, b, c) = V.fromList [toField a, toField b, toField c]

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

instance (ToField a, ToField b, ToField c, ToField d) =>
         ToRecord (a, b, c, d) where
    toRecord (a, b, c, d) = V.fromList [
        toField a, toField b, toField c, toField d]

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

instance (ToField a, ToField b, ToField c, ToField d, ToField e) =>
         ToRecord (a, b, c, d, e) where
    toRecord (a, b, c, d, e) = V.fromList [
        toField a, toField b, toField c, toField d, toField e]

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

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f) =>
         ToRecord (a, b, c, d, e, f) where
    toRecord (a, b, c, d, e, f) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f]

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

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f, ToField g) =>
         ToRecord (a, b, c, d, e, f, g) where
    toRecord (a, b, c, d, e, f, g) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g]

instance FromField a => FromRecord [a] where
    parseRecord = traverse parseField . V.toList

instance FromField a => FromRecord (V.Vector a) where
    parseRecord = traverse parseField

-- | A type that can be converted from a single CSV field, with the
-- possibility of failure.
--
-- When writing an instance, use 'empty', 'mzero', or 'fail' to make a
-- conversion fail, e.g. if a 'Field' can't be converted to the given
-- type.
--
-- Example type and instance:
--
-- @{-\# LANGUAGE OverloadedStrings \#-}
--
-- data Color = Red | Green | Blue
--
-- instance FromField Color where
--     parseField s
--         | s == \"R\"  = pure Red
--         | s == \"G\"  = pure Green
--         | s == \"B\"  = pure Blue
--         | otherwise = mzero
-- @
class FromField a where
    parseField :: Field -> Parser a

instance FromField Char where
    parseField s
        | T.compareLength t 1 == EQ = pure (T.head t)
        | otherwise = fail $ "when expecting a Char, encountered \"" ++
                              B8.unpack s ++ "\" instead"
      where t = T.decodeUtf8 s
    {-# INLINE parseField #-}

instance FromField Double where
    parseField = parseDouble
    {-# INLINE parseField #-}

instance FromField Float where
    parseField s = double2Float <$> parseDouble s
    {-# INLINE parseField #-}

parseDouble :: S.ByteString -> Parser Double
parseDouble s = case parseOnly double s of
    Left err -> fail err
    Right n  -> pure n
{-# INLINE parseDouble #-}

instance FromField Int where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Integer where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Int8 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Int16 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Int32 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Int64 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Word where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Word8 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Word16 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Word32 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField Word64 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance FromField S.ByteString where
    parseField = pure
    {-# INLINE parseField #-}

instance FromField L.ByteString where
    parseField s = pure (L.fromChunks [s])
    {-# INLINE parseField #-}

instance FromField T.Text where
    parseField = pure . T.decodeUtf8
    {-# INLINE parseField #-}

instance FromField LT.Text where
    parseField s = pure (LT.fromChunks [T.decodeUtf8 s])
    {-# INLINE parseField #-}

parseIntegral :: Integral a => S.ByteString -> Parser a
parseIntegral s = case parseOnly number s of
    Left err -> fail err
    Right n  -> pure (floor n)
{-# INLINE parseIntegral #-}

------------------------------------------------------------------------
-- Constructors and accessors

-- | Retrieve the /n/th field in the given record.  The result is
-- 'empty' if the value cannot be converted to the desired type.
(.!) :: FromField a => Record -> Int -> Parser a
v .! idx = parseField (v ! idx)
{-# INLINE (.!) #-}
