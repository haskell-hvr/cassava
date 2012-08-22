{-# LANGUAGE OverloadedStrings #-}
module Data.Ceason.Types.Class
    (
    -- * Type conversion
      Only(..)
    , FromRecord(..)
    , BSMap(..)
    , BSHashMap(..)
    , FromNamedRecord(..)
    , ToNamedRecord(..)
    , FromField(..)
    , ToRecord(..)
    , ToField(..)

    -- * Accessors
    , (.!)
    , (.:)
    , (.=)
    , record
    , namedRecord
    ) where

import Control.Applicative
import Data.Attoparsec.Char8 (double, number, parseOnly)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Lazy as HM
import Data.Int
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Traversable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Word
import GHC.Float (double2Float)
import Prelude hiding (takeWhile)

import Data.Ceason.Encode.Internal
import Data.Ceason.Types.Internal

------------------------------------------------------------------------
-- Type conversion

------------------------------------------------------------------------
-- Index-based conversion

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

-- | A type that can be converted to a single CSV record.
--
-- An example type and instance:
--
-- @data Person = Person { name :: Text, age :: Int }
--
-- instance ToRecord Person where
--     toRecord (Person name age) = record [
--        toField name, toField age]
-- @
--
-- Outputs data on this form:
--
-- > John,56
-- > Jane,55
class ToRecord a where
    toRecord :: a -> Record

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

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f) =>
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

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g) =>
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

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g) =>
         ToRecord (a, b, c, d, e, f, g) where
    toRecord (a, b, c, d, e, f, g) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g]

instance FromField a => FromRecord [a] where
    parseRecord = traverse parseField . V.toList

instance ToField a => ToRecord [a] where
    toRecord = V.fromList . map toField

instance FromField a => FromRecord (V.Vector a) where
    parseRecord = traverse parseField

-- TODO: Check if this can give rise to overlapping instances.
instance ToField a => ToRecord (Vector a) where
    toRecord = V.map toField

------------------------------------------------------------------------
-- Name-based conversion

-- | A 'M.Map' keyed by 'B.ByteString' keys.
--
-- The primary use case of 'BSMap' is to decode a CSV file into a
-- @'BSMap' 'B.ByteString'@, which lets you process the CSV data
-- without converting it to a more specific type.
newtype BSMap a = BSMap {
      fromBSMap :: M.Map B.ByteString a
    } deriving (Eq, Ord, Read, Show)

-- | A 'HM.HashMap' keyed by 'B.ByteString' keys.
--
-- The primary use case of 'BSHashMap' is to decode a CSV file into a
-- @'BSHashMap' 'B.ByteString'@, which lets you process the CSV data
-- without converting it to a more specific type.
newtype BSHashMap a = BSHashMap {
      fromBSHashMap :: HM.HashMap B.ByteString a
    } deriving (Eq, Show)

-- | A type that can be converted from a single CSV record, with the
-- possibility of failure.
--
-- When writing an instance, use 'empty', 'mzero', or 'fail' to make a
-- conversion fail, e.g. if a 'Record' has the wrong number of
-- columns.
--
-- Given this example data:
--
-- > name,age
-- > John,56
-- > Jane,55
--
-- here's an example type and instance:
--
-- @{-\# LANGUAGE OverloadedStrings \#-}
--
-- data Person = Person { name :: Text, age :: Int }
--
-- instance FromRecord Person where
--     parseNamedRecord m = Person '<$>'
--                          m '.:' \"name\" '<*>'
--                          m '.:' \"age\"
-- @
--
-- Note the use of the @OverloadedStrings@ language extension which
-- enables 'B8.ByteString' values to be written as string literals.
class FromNamedRecord a where
    parseNamedRecord :: NamedRecord -> Parser a

-- | A type that can be converted to a single CSV record.
--
-- An example type and instance:
--
-- @data Person = Person { name :: Text, age :: Int }
--
-- instance ToRecord Person where
--     toNamedRecord (Person name age) = 'namedRecord' [
--        \"name\" '.=' name, \"age\" '.=' age]
-- @
class ToNamedRecord a where
    toNamedRecord :: a -> NamedRecord

instance FromField a => FromNamedRecord (BSMap a) where
    parseNamedRecord m = BSMap . M.fromList <$>
                         (traverse parseSnd $ HM.toList m)
      where parseSnd (name, s) = (,) <$> pure name <*> parseField s

instance ToField a => ToNamedRecord (BSMap a) where
    toNamedRecord = HM.fromList . map (\ (k, v) -> (k, toField v)) . M.toList .
                    fromBSMap

instance FromField a => FromNamedRecord (BSHashMap a) where
    parseNamedRecord m = BSHashMap <$> traverse (\ s -> parseField s) m

instance ToField a => ToNamedRecord (BSHashMap a) where
    toNamedRecord = HM.map toField . fromBSHashMap

------------------------------------------------------------------------
-- Individual field conversion

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

class ToField a where
    toField :: a -> Field

instance FromField Char where
    parseField s
        | T.compareLength t 1 == EQ = pure (T.head t)
        | otherwise = fail $ "when expecting a Char, encountered \"" ++
                              B8.unpack s ++ "\" instead"
      where t = T.decodeUtf8 s
    {-# INLINE parseField #-}

instance ToField Char where
    toField = toField . T.encodeUtf8 . T.singleton
    {-# INLINE toField #-}

instance FromField Double where
    parseField = parseDouble
    {-# INLINE parseField #-}

instance ToField Double where
    toField = realFloat
    {-# INLINE toField #-}

instance FromField Float where
    parseField s = double2Float <$> parseDouble s
    {-# INLINE parseField #-}

instance ToField Float where
    toField = realFloat
    {-# INLINE toField #-}

parseDouble :: B.ByteString -> Parser Double
parseDouble s = case parseOnly double s of
    Left err -> fail err
    Right n  -> pure n
{-# INLINE parseDouble #-}

instance FromField Int where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Int where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Integer where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Integer where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Int8 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Int8 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Int16 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Int16 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Int32 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Int32 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Int64 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Int64 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Word where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word8 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Word8 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word16 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Word16 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word32 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Word32 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word64 where
    parseField = parseIntegral
    {-# INLINE parseField #-}

instance ToField Word64 where
    toField = decimal
    {-# INLINE toField #-}

-- TODO: Optimize
escape :: B.ByteString -> B.ByteString
escape s
    | B.find (\ b -> b == dquote || b == comma || b == nl || b == cr ||
                     b == sp) s == Nothing = s
    | otherwise =
        B.concat ["\"",
                  B.concatMap
                  (\ b -> if b == dquote then "\"\"" else B.singleton b) s,
                  "\""]
  where
    dquote = 34
    comma  = 44
    nl     = 10
    cr     = 13
    sp     = 32

instance FromField B.ByteString where
    parseField = pure
    {-# INLINE parseField #-}

instance ToField B.ByteString where
    toField = escape
    {-# INLINE toField #-}

instance FromField L.ByteString where
    parseField s = pure (L.fromChunks [s])
    {-# INLINE parseField #-}

instance ToField L.ByteString where
    toField = toField . B.concat . L.toChunks
    {-# INLINE toField #-}

instance FromField T.Text where
    parseField = pure . T.decodeUtf8
    {-# INLINE parseField #-}

instance ToField T.Text where
    toField = toField . T.encodeUtf8
    {-# INLINE toField #-}

instance FromField LT.Text where
    parseField s = pure (LT.fromChunks [T.decodeUtf8 s])
    {-# INLINE parseField #-}

instance ToField LT.Text where
    toField = toField . B.concat . L.toChunks . LT.encodeUtf8
    {-# INLINE toField #-}

parseIntegral :: Integral a => B.ByteString -> Parser a
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

-- | Retrieve a field in the given record by name.  The result is
-- 'empty' if the field is missing or if the value cannot be converted
-- to the desired type.
(.:) :: FromField a => NamedRecord -> B.ByteString -> Parser a
m .: name = maybe (fail err) parseField $ HM.lookup name m
  where err = "No field named " ++ B8.unpack name
{-# INLINE (.:) #-}

-- | Construct a pair from a name and a value.  For use with
-- 'namedRecord'.
(.=) :: ToField a => B.ByteString -> a -> (B.ByteString, B.ByteString)
name .= val = (name, toField val)
{-# INLINE (.=) #-}

-- | Construct a record from a list of 'B.ByteString's.  Use 'toField'
-- to convert values to 'B.ByteString's for use with 'record'.
record :: [B.ByteString] -> Record
record = V.fromList

-- | Construct a named record from a list of name-value 'B.ByteString'
-- pairs.  Use '.=' to construct such a pair from a name and a value.
namedRecord :: [(B.ByteString, B.ByteString)] -> NamedRecord
namedRecord = HM.fromList
