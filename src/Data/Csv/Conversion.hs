{-# LANGUAGE
    BangPatterns,
    CPP,
    DefaultSignatures,
    FlexibleContexts,
    FlexibleInstances,
    KindSignatures,
    MultiParamTypeClasses,
    OverloadedStrings,
    Rank2Types,
    ScopedTypeVariables,
    TypeOperators,
    UndecidableInstances
    #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#endif

#if !MIN_VERSION_bytestring(0,10,4)
# define MIN_VERSION_text_short(a,b,c) 0
#endif

#if !defined(MIN_VERSION_text_short)
# error **INVARIANT BROKEN** Detected invalid combination of `text-short` and `bytestring` versions. Please verify the `pre-bytestring-0.10-4` flag-logic in the .cabal file wasn't elided.
#endif

module Data.Csv.Conversion
    (
    -- * Type conversion
      Only(..)
    , FromRecord(..)
    , FromNamedRecord(..)
    , ToNamedRecord(..)
    , DefaultOrdered(..)
    , FromField(..)
    , ToRecord(..)
    , ToField(..)

    -- ** Generic type conversion
    , genericParseRecord
    , genericToRecord
    , genericParseNamedRecord
    , genericToNamedRecord
    , genericHeaderOrder

    -- *** Generic type conversion options
    , Options
    , defaultOptions
    , fieldLabelModifier

    -- *** Generic type conversion class names
    , GFromRecord
    , GToRecord
    , GFromNamedRecord
    , GToNamedRecordHeader

    -- * Parser
    , Parser
    , runParser

    -- * Accessors
    , index
    , (.!)
    , unsafeIndex
    , lookup
    , (.:)
    , namedField
    , (.=)
    , record
    , namedRecord
    , header
    ) where

import Control.Applicative (Alternative, (<|>), empty, Const(..))
import Control.Monad (MonadPlus, mplus, mzero)
import qualified Control.Monad.Fail as Fail
import Data.Attoparsec.ByteString.Char8 (double)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short as SBS
#endif
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HM
import Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Scientific (Scientific)
import Data.Semigroup as Semi (Semigroup, (<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
#if MIN_VERSION_text_short(0,1,0)
import qualified Data.Text.Short as T.S
#endif
import Data.Tuple.Only (Only(..))
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Float (double2Float)
import GHC.Generics
import Numeric.Natural
import Prelude hiding (lookup, takeWhile)

import Data.Csv.Conversion.Internal
import Data.Csv.Types

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>), (<*>), (<*), (*>), pure)
import Data.Monoid (Monoid, mappend, mempty)
import Data.Traversable (traverse)
import Data.Word (Word)
#endif

------------------------------------------------------------------------
-- bytestring compatibility

toStrict   :: L.ByteString -> B.ByteString
fromStrict :: B.ByteString -> L.ByteString
#if MIN_VERSION_bytestring(0,10,0)
toStrict   = L.toStrict
fromStrict = L.fromStrict
#else
toStrict   = B.concat . L.toChunks
fromStrict = L.fromChunks . (:[])
#endif
{-# INLINE toStrict #-}
{-# INLINE fromStrict #-}

------------------------------------------------------------------------
-- Type conversion

------------------------------------------------------------------------
-- Index-based conversion

-- | Options to customise how to generically encode\/decode your
--   datatype to\/from CSV.
--
--   @since 0.5.1.0
newtype Options = Options
  { fieldLabelModifier :: String -> String
    -- ^ How to convert Haskell field labels to CSV fields.
    --
    --   @since 0.5.1.0
  }

instance Show Options where
  show (Options fld) =
    "Options {"
      ++ intercalate ","
         [ "fieldLabelModifier =~ " ++ show sampleField ++ " -> " ++ show (fld sampleField)
         ]
      ++ "}"
    where
      sampleField = "_column_A"

-- | Default conversion options.
--
--   @
--   Options
--   { 'fieldLabelModifier' = id
--   }
--   @
--
--   @since 0.5.1.0
defaultOptions :: Options
defaultOptions = Options
  { fieldLabelModifier = id
  }

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
-- > data Person = Person { name :: !Text, age :: !Int }
-- >
-- > instance FromRecord Person where
-- >     parseRecord v
-- >         | length v == 2 = Person <$>
-- >                           v .! 0 <*>
-- >                           v .! 1
-- >         | otherwise     = mzero
class FromRecord a where
    parseRecord :: Record -> Parser a

    default parseRecord :: (Generic a, GFromRecord (Rep a)) => Record -> Parser a
    parseRecord = genericParseRecord defaultOptions

-- | A configurable CSV record parser.  This function applied to
--   'defaultOptions' is used as the default for 'parseRecord' when the
--   type is an instance of 'Generic'.
--
--   @since 0.5.1.0
genericParseRecord :: (Generic a, GFromRecord (Rep a)) => Options -> Record -> Parser a
genericParseRecord opts r = to <$> gparseRecord opts r

-- | A type that can be converted to a single CSV record.
--
-- An example type and instance:
--
-- > data Person = Person { name :: !Text, age :: !Int }
-- >
-- > instance ToRecord Person where
-- >     toRecord (Person name age) = record [
-- >         toField name, toField age]
--
-- Outputs data on this form:
--
-- > John,56
-- > Jane,55
class ToRecord a where
    -- | Convert a value to a record.
    toRecord :: a -> Record

    default toRecord :: (Generic a, GToRecord (Rep a) Field) => a -> Record
    toRecord = genericToRecord defaultOptions

-- | A configurable CSV record creator.  This function applied to
--   'defaultOptions' is used as the default for 'toRecord' when the
--   type is an instance of 'Generic'.
--
--   @since 0.5.1.0
genericToRecord :: (Generic a, GToRecord (Rep a) Field) => Options -> a -> Record
genericToRecord opts = V.fromList . gtoRecord opts . from

instance FromField a => FromRecord (Only a) where
    parseRecord v
        | n == 1    = Only <$> unsafeIndex v 0
        | otherwise = lengthMismatch 1 v
          where
            n = V.length v

-- TODO: Check if we want all toRecord conversions to be stricter.

instance ToField a => ToRecord (Only a) where
    toRecord = V.singleton . toField . fromOnly

instance (FromField a, FromField b) => FromRecord (a, b) where
    parseRecord v
        | n == 2    = (,) <$> unsafeIndex v 0
                          <*> unsafeIndex v 1
        | otherwise = lengthMismatch 2 v
          where
            n = V.length v

instance (ToField a, ToField b) => ToRecord (a, b) where
    toRecord (a, b) = V.fromList [toField a, toField b]

instance (FromField a, FromField b, FromField c) => FromRecord (a, b, c) where
    parseRecord v
        | n == 3    = (,,) <$> unsafeIndex v 0
                           <*> unsafeIndex v 1
                           <*> unsafeIndex v 2
        | otherwise = lengthMismatch 3 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c) =>
         ToRecord (a, b, c) where
    toRecord (a, b, c) = V.fromList [toField a, toField b, toField c]

instance (FromField a, FromField b, FromField c, FromField d) =>
         FromRecord (a, b, c, d) where
    parseRecord v
        | n == 4    = (,,,) <$> unsafeIndex v 0
                            <*> unsafeIndex v 1
                            <*> unsafeIndex v 2
                            <*> unsafeIndex v 3
        | otherwise = lengthMismatch 4 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d) =>
         ToRecord (a, b, c, d) where
    toRecord (a, b, c, d) = V.fromList [
        toField a, toField b, toField c, toField d]

instance (FromField a, FromField b, FromField c, FromField d, FromField e) =>
         FromRecord (a, b, c, d, e) where
    parseRecord v
        | n == 5    = (,,,,) <$> unsafeIndex v 0
                             <*> unsafeIndex v 1
                             <*> unsafeIndex v 2
                             <*> unsafeIndex v 3
                             <*> unsafeIndex v 4
        | otherwise = lengthMismatch 5 v
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
        | n == 6    = (,,,,,) <$> unsafeIndex v 0
                              <*> unsafeIndex v 1
                              <*> unsafeIndex v 2
                              <*> unsafeIndex v 3
                              <*> unsafeIndex v 4
                              <*> unsafeIndex v 5
        | otherwise = lengthMismatch 6 v
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
        | n == 7    = (,,,,,,) <$> unsafeIndex v 0
                               <*> unsafeIndex v 1
                               <*> unsafeIndex v 2
                               <*> unsafeIndex v 3
                               <*> unsafeIndex v 4
                               <*> unsafeIndex v 5
                               <*> unsafeIndex v 6
        | otherwise = lengthMismatch 7 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g) =>
         ToRecord (a, b, c, d, e, f, g) where
    toRecord (a, b, c, d, e, f, g) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g]

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h) =>
         FromRecord (a, b, c, d, e, f, g, h) where
    parseRecord v
        | n == 8    = (,,,,,,,) <$> unsafeIndex v 0
                                <*> unsafeIndex v 1
                                <*> unsafeIndex v 2
                                <*> unsafeIndex v 3
                                <*> unsafeIndex v 4
                                <*> unsafeIndex v 5
                                <*> unsafeIndex v 6
                                <*> unsafeIndex v 7
        | otherwise = lengthMismatch 8 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h) =>
         ToRecord (a, b, c, d, e, f, g, h) where
    toRecord (a, b, c, d, e, f, g, h) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g, toField h]

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i) =>
         FromRecord (a, b, c, d, e, f, g, h, i) where
    parseRecord v
        | n == 9    = (,,,,,,,,) <$> unsafeIndex v 0
                                 <*> unsafeIndex v 1
                                 <*> unsafeIndex v 2
                                 <*> unsafeIndex v 3
                                 <*> unsafeIndex v 4
                                 <*> unsafeIndex v 5
                                 <*> unsafeIndex v 6
                                 <*> unsafeIndex v 7
                                 <*> unsafeIndex v 8
        | otherwise = lengthMismatch 9 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i) =>
         ToRecord (a, b, c, d, e, f, g, h, i) where
    toRecord (a, b, c, d, e, f, g, h, i) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g, toField h, toField i]

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j) =>
         FromRecord (a, b, c, d, e, f, g, h, i, j) where
    parseRecord v
        | n == 10    = (,,,,,,,,,) <$> unsafeIndex v 0
                                   <*> unsafeIndex v 1
                                   <*> unsafeIndex v 2
                                   <*> unsafeIndex v 3
                                   <*> unsafeIndex v 4
                                   <*> unsafeIndex v 5
                                   <*> unsafeIndex v 6
                                   <*> unsafeIndex v 7
                                   <*> unsafeIndex v 8
                                   <*> unsafeIndex v 9
        | otherwise = lengthMismatch 10 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j) =>
         ToRecord (a, b, c, d, e, f, g, h, i, j) where
    toRecord (a, b, c, d, e, f, g, h, i, j) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g, toField h, toField i, toField j]

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k) =>
         FromRecord (a, b, c, d, e, f, g, h, i, j, k) where
    parseRecord v
        | n == 11    = (,,,,,,,,,,) <$> unsafeIndex v 0
                                    <*> unsafeIndex v 1
                                    <*> unsafeIndex v 2
                                    <*> unsafeIndex v 3
                                    <*> unsafeIndex v 4
                                    <*> unsafeIndex v 5
                                    <*> unsafeIndex v 6
                                    <*> unsafeIndex v 7
                                    <*> unsafeIndex v 8
                                    <*> unsafeIndex v 9
                                    <*> unsafeIndex v 10
        | otherwise = lengthMismatch 11 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k) =>
         ToRecord (a, b, c, d, e, f, g, h, i, j, k) where
    toRecord (a, b, c, d, e, f, g, h, i, j, k) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g, toField h, toField i, toField j, toField k]

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l) =>
         FromRecord (a, b, c, d, e, f, g, h, i, j, k, l) where
    parseRecord v
        | n == 12    = (,,,,,,,,,,,) <$> unsafeIndex v 0
                                     <*> unsafeIndex v 1
                                     <*> unsafeIndex v 2
                                     <*> unsafeIndex v 3
                                     <*> unsafeIndex v 4
                                     <*> unsafeIndex v 5
                                     <*> unsafeIndex v 6
                                     <*> unsafeIndex v 7
                                     <*> unsafeIndex v 8
                                     <*> unsafeIndex v 9
                                     <*> unsafeIndex v 10
                                     <*> unsafeIndex v 11
        | otherwise = lengthMismatch 12 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l) =>
         ToRecord (a, b, c, d, e, f, g, h, i, j, k, l) where
    toRecord (a, b, c, d, e, f, g, h, i, j, k, l) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g, toField h, toField i, toField j, toField k, toField l]

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m) =>
         FromRecord (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    parseRecord v
        | n == 13    = (,,,,,,,,,,,,) <$> unsafeIndex v 0
                                      <*> unsafeIndex v 1
                                      <*> unsafeIndex v 2
                                      <*> unsafeIndex v 3
                                      <*> unsafeIndex v 4
                                      <*> unsafeIndex v 5
                                      <*> unsafeIndex v 6
                                      <*> unsafeIndex v 7
                                      <*> unsafeIndex v 8
                                      <*> unsafeIndex v 9
                                      <*> unsafeIndex v 10
                                      <*> unsafeIndex v 11
                                      <*> unsafeIndex v 12
        | otherwise = lengthMismatch 13 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m) =>
         ToRecord (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    toRecord (a, b, c, d, e, f, g, h, i, j, k, l, m) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g, toField h, toField i, toField j, toField k, toField l,
        toField m]

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m, FromField n) =>
         FromRecord (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    parseRecord v
        | n == 14    = (,,,,,,,,,,,,,) <$> unsafeIndex v 0
                                       <*> unsafeIndex v 1
                                       <*> unsafeIndex v 2
                                       <*> unsafeIndex v 3
                                       <*> unsafeIndex v 4
                                       <*> unsafeIndex v 5
                                       <*> unsafeIndex v 6
                                       <*> unsafeIndex v 7
                                       <*> unsafeIndex v 8
                                       <*> unsafeIndex v 9
                                       <*> unsafeIndex v 10
                                       <*> unsafeIndex v 11
                                       <*> unsafeIndex v 12
                                       <*> unsafeIndex v 13
        | otherwise = lengthMismatch 14 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n) =>
         ToRecord (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    toRecord (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g, toField h, toField i, toField j, toField k, toField l,
        toField m, toField n]

instance (FromField a, FromField b, FromField c, FromField d, FromField e,
          FromField f, FromField g, FromField h, FromField i, FromField j,
          FromField k, FromField l, FromField m, FromField n, FromField o) =>
         FromRecord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    parseRecord v
        | n == 15    = (,,,,,,,,,,,,,,) <$> unsafeIndex v 0
                                        <*> unsafeIndex v 1
                                        <*> unsafeIndex v 2
                                        <*> unsafeIndex v 3
                                        <*> unsafeIndex v 4
                                        <*> unsafeIndex v 5
                                        <*> unsafeIndex v 6
                                        <*> unsafeIndex v 7
                                        <*> unsafeIndex v 8
                                        <*> unsafeIndex v 9
                                        <*> unsafeIndex v 10
                                        <*> unsafeIndex v 11
                                        <*> unsafeIndex v 12
                                        <*> unsafeIndex v 13
                                        <*> unsafeIndex v 14
        | otherwise = lengthMismatch 15 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g, ToField h, ToField i, ToField j, ToField k, ToField l,
          ToField m, ToField n, ToField o) =>
         ToRecord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    toRecord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g, toField h, toField i, toField j, toField k, toField l,
        toField m, toField n, toField o]

lengthMismatch :: Int -> Record -> Parser a
lengthMismatch expected v =
    fail $ "cannot unpack array of length " ++
    show n ++ " into a " ++ desired ++ ". Input record: " ++
    show v
  where
    n = V.length v
    desired | expected == 1 = "Only"
            | expected == 2 = "pair"
            | otherwise     = show expected ++ "-tuple"

instance FromField a => FromRecord [a] where
    parseRecord = traverse parseField . V.toList

instance ToField a => ToRecord [a] where
    toRecord = V.fromList . map toField

instance FromField a => FromRecord (V.Vector a) where
    parseRecord = traverse parseField

instance ToField a => ToRecord (Vector a) where
    toRecord = V.map toField

instance (FromField a, U.Unbox a) => FromRecord (U.Vector a) where
    parseRecord = fmap U.convert . traverse parseField

instance (ToField a, U.Unbox a) => ToRecord (U.Vector a) where
    toRecord = V.map toField . U.convert

------------------------------------------------------------------------
-- Name-based conversion

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
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > data Person = Person { name :: !Text, age :: !Int }
-- >
-- > instance FromNamedRecord Person where
-- >     parseNamedRecord m = Person <$>
-- >                          m .: "name" <*>
-- >                          m .: "age"
--
-- Note the use of the @OverloadedStrings@ language extension which
-- enables 'B8.ByteString' values to be written as string literals.
class FromNamedRecord a where
    parseNamedRecord :: NamedRecord -> Parser a

    default parseNamedRecord :: (Generic a, GFromNamedRecord (Rep a)) => NamedRecord -> Parser a
    parseNamedRecord = genericParseNamedRecord defaultOptions

-- | A configurable CSV named record parser.  This function applied to
--   'defaultOptions' is used as the default for 'parseNamedRecord'
--   when the type is an instance of 'Generic'.
--
--   @since 0.5.1.0
genericParseNamedRecord :: (Generic a, GFromNamedRecord (Rep a)) => Options -> NamedRecord -> Parser a
genericParseNamedRecord opts r = to <$> gparseNamedRecord opts r

-- | A type that can be converted to a single CSV record.
--
-- An example type and instance:
--
-- > data Person = Person { name :: !Text, age :: !Int }
-- >
-- > instance ToNamedRecord Person where
-- >     toNamedRecord (Person name age) = namedRecord [
-- >         "name" .= name, "age" .= age]
class ToNamedRecord a where
    -- | Convert a value to a named record.
    toNamedRecord :: a -> NamedRecord

    default toNamedRecord ::
        (Generic a, GToRecord (Rep a) (B.ByteString, B.ByteString)) =>
        a -> NamedRecord
    toNamedRecord = genericToNamedRecord defaultOptions

-- | A configurable CSV named record creator.  This function applied
--   to 'defaultOptions' is used as the default for 'toNamedRecord' when
--   the type is an instance of 'Generic'.
--
--   @since 0.5.1.0
genericToNamedRecord :: (Generic a, GToRecord (Rep a) (B.ByteString, B.ByteString))
                        => Options -> a -> NamedRecord
genericToNamedRecord opts = namedRecord . gtoRecord opts . from

-- | A type that has a default field order when converted to CSV. This
-- class lets you specify how to get the headers to use for a record
-- type that's an instance of 'ToNamedRecord'.
--
-- To derive an instance, the type is required to only have one
-- constructor and that constructor must have named fields (also known
-- as selectors) for all fields.
--
-- Right: @data Foo = Foo { foo :: !Int }@
--
-- Wrong: @data Bar = Bar Int@
--
-- If you try to derive an instance using GHC generics and your type
-- doesn't have named fields, you will get an error along the lines
-- of:
--
-- > <interactive>:9:10:
-- >     No instance for (DefaultOrdered (M1 S NoSelector (K1 R Char) ()))
-- >       arising from a use of ‘Data.Csv.Conversion.$gdmheader’
-- >     In the expression: Data.Csv.Conversion.$gdmheader
-- >     In an equation for ‘header’:
-- >         header = Data.Csv.Conversion.$gdmheader
-- >     In the instance declaration for ‘DefaultOrdered Foo’
--
class DefaultOrdered a where
    -- | The header order for this record. Should include the names
    -- used in the 'NamedRecord' returned by 'toNamedRecord'. Pass
    -- 'undefined' as the argument, together with a type annotation
    -- e.g. @'headerOrder' ('undefined' :: MyRecord)@.
    headerOrder :: a -> Header  -- TODO: Add Generic implementation

    default headerOrder ::
        (Generic a, GToNamedRecordHeader (Rep a)) =>
        a -> Header
    headerOrder = genericHeaderOrder defaultOptions

-- | A configurable CSV header record generator.  This function
--   applied to 'defaultOptions' is used as the default for
--   'headerOrder' when the type is an instance of 'Generic'.
--
--   @since 0.5.1.0
genericHeaderOrder :: (Generic a, GToNamedRecordHeader (Rep a))
                      => Options -> a -> Header
genericHeaderOrder opts = V.fromList. gtoNamedRecordHeader opts . from

instance (FromField a, FromField b, Ord a) => FromNamedRecord (M.Map a b) where
    parseNamedRecord m = M.fromList <$>
                         (traverse parseBoth $ HM.toList m)

instance (ToField a, ToField b, Ord a) => ToNamedRecord (M.Map a b) where
    toNamedRecord = HM.fromList . map (\ (k, v) -> (toField k, toField v)) . M.toList

instance (Eq a, FromField a, FromField b, Hashable a) => FromNamedRecord (HM.HashMap a b) where
    parseNamedRecord m = HM.fromList <$>
                         (traverse parseBoth $ HM.toList m)

instance (Eq a, ToField a, ToField b, Hashable a) => ToNamedRecord (HM.HashMap a b) where
    toNamedRecord = HM.fromList . map (\ (k, v) -> (toField k, toField v)) . HM.toList

parseBoth :: (FromField a, FromField b) => (Field, Field) -> Parser (a, b)
parseBoth (k, v) = (,) <$> parseField k <*> parseField v

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
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > data Color = Red | Green | Blue
-- >
-- > instance FromField Color where
-- >     parseField s
-- >         | s == "R"  = pure Red
-- >         | s == "G"  = pure Green
-- >         | s == "B"  = pure Blue
-- >         | otherwise = mzero
class FromField a where
    parseField :: Field -> Parser a

-- | A type that can be converted to a single CSV field.
--
-- Example type and instance:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > data Color = Red | Green | Blue
-- >
-- > instance ToField Color where
-- >     toField Red   = "R"
-- >     toField Green = "G"
-- >     toField Blue  = "B"
class ToField a where
    toField :: a -> Field

-- | 'Nothing' if the 'Field' is 'B.empty', 'Just' otherwise.
instance FromField a => FromField (Maybe a) where
    parseField s
        | B.null s  = pure Nothing
        | otherwise = Just <$> parseField s
    {-# INLINE parseField #-}

-- | 'Nothing' is encoded as an 'B.empty' field.
instance ToField a => ToField (Maybe a) where
    toField = maybe B.empty toField
    {-# INLINE toField #-}

-- | @'Left' field@ if conversion failed, 'Right' otherwise.
instance FromField a => FromField (Either Field a) where
    parseField s = case runParser (parseField s) of
        Left _  -> pure $ Left s
        Right a -> pure $ Right a
    {-# INLINE parseField #-}

-- | Ignores the 'Field'. Always succeeds.
instance FromField () where
    parseField _ = pure ()
    {-# INLINE parseField #-}

-- | @since 0.5.2.0
instance FromField a => FromField (Identity a) where
    parseField = fmap Identity . parseField
    {-# INLINE parseField #-}

-- | @since 0.5.2.0
instance ToField a => ToField (Identity a) where
    toField = toField . runIdentity
    {-# INLINE toField #-}

-- | @since 0.5.2.0
instance FromField a => FromField (Const a b) where
    parseField = fmap Const . parseField
    {-# INLINE parseField #-}

-- | @since 0.5.2.0
instance ToField a => ToField (Const a b) where
    toField = toField . getConst
    {-# INLINE toField #-}

-- | Assumes UTF-8 encoding.
instance FromField Char where
    parseField s =
        case T.decodeUtf8' s of
          Left e -> fail $ show e
          Right t
            | T.compareLength t 1 == EQ -> pure (T.head t)
            | otherwise -> typeError "Char" s Nothing
    {-# INLINE parseField #-}

-- | Uses UTF-8 encoding.
instance ToField Char where
    toField = toField . T.encodeUtf8 . T.singleton
    {-# INLINE toField #-}

-- | Accepts the same syntax as 'rational'. Ignores whitespace.
--
-- @since 0.5.1.0
instance FromField Scientific where
  parseField s = case parseOnly (ws *> A8.scientific <* ws) s of
                   Left err -> typeError "Scientific" s (Just err)
                   Right n  -> pure n
  {-# INLINE parseField #-}

-- | Uses decimal notation or scientific notation, depending on the number.
--
-- @since 0.5.1.0
instance ToField Scientific where
  toField = scientific
  {-# INLINE toField #-}

-- | Accepts same syntax as 'rational'. Ignores whitespace.
instance FromField Double where
    parseField = parseDouble
    {-# INLINE parseField #-}

-- | Uses decimal notation or scientific notation, depending on the
-- number.
instance ToField Double where
    toField = realFloat
    {-# INLINE toField #-}

-- | Accepts same syntax as 'rational'. Ignores whitespace.
instance FromField Float where
    parseField s = double2Float <$> parseDouble s
    {-# INLINE parseField #-}

-- | Uses decimal notation or scientific notation, depending on the
-- number.
instance ToField Float where
    toField = realFloat
    {-# INLINE toField #-}

parseDouble :: B.ByteString -> Parser Double
parseDouble s = case parseOnly (ws *> double <* ws) s of
    Left err -> typeError "Double" s (Just err)
    Right n  -> pure n
{-# INLINE parseDouble #-}

-- | Accepts a signed decimal number. Ignores whitespace.
instance FromField Int where
    parseField = parseSigned "Int"
    {-# INLINE parseField #-}

-- | Uses decimal encoding with optional sign.
instance ToField Int where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts a signed decimal number. Ignores whitespace.
instance FromField Integer where
    parseField = parseSigned "Integer"
    {-# INLINE parseField #-}

-- | Uses decimal encoding with optional sign.
instance ToField Integer where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts a signed decimal number. Ignores whitespace.
instance FromField Int8 where
    parseField = parseSigned "Int8"
    {-# INLINE parseField #-}

-- | Uses decimal encoding with optional sign.
instance ToField Int8 where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts a signed decimal number. Ignores whitespace.
instance FromField Int16 where
    parseField = parseSigned "Int16"
    {-# INLINE parseField #-}

-- | Uses decimal encoding with optional sign.
instance ToField Int16 where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts a signed decimal number. Ignores whitespace.
instance FromField Int32 where
    parseField = parseSigned "Int32"
    {-# INLINE parseField #-}

-- | Uses decimal encoding with optional sign.
instance ToField Int32 where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts a signed decimal number. Ignores whitespace.
instance FromField Int64 where
    parseField = parseSigned "Int64"
    {-# INLINE parseField #-}

-- | Uses decimal encoding with optional sign.
instance ToField Int64 where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts an unsigned decimal number. Ignores whitespace.
instance FromField Word where
    parseField = parseUnsigned "Word"
    {-# INLINE parseField #-}

-- | Uses decimal encoding.
instance ToField Word where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts an unsigned decimal number. Ignores whitespace.
--
-- @since 0.5.1.0
instance FromField Natural where
    parseField = parseUnsigned "Natural"
    {-# INLINE parseField #-}

-- | Uses decimal encoding.
--
-- @since 0.5.1.0
instance ToField Natural where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts an unsigned decimal number. Ignores whitespace.
instance FromField Word8 where
    parseField = parseUnsigned "Word8"
    {-# INLINE parseField #-}

-- | Uses decimal encoding.
instance ToField Word8 where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts an unsigned decimal number. Ignores whitespace.
instance FromField Word16 where
    parseField = parseUnsigned "Word16"
    {-# INLINE parseField #-}

-- | Uses decimal encoding.
instance ToField Word16 where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts an unsigned decimal number. Ignores whitespace.
instance FromField Word32 where
    parseField = parseUnsigned "Word32"
    {-# INLINE parseField #-}

-- | Uses decimal encoding.
instance ToField Word32 where
    toField = decimal
    {-# INLINE toField #-}

-- | Accepts an unsigned decimal number. Ignores whitespace.
instance FromField Word64 where
    parseField = parseUnsigned "Word64"
    {-# INLINE parseField #-}

-- | Uses decimal encoding.
instance ToField Word64 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField B.ByteString where
    parseField = pure
    {-# INLINE parseField #-}

instance ToField B.ByteString where
    toField = id
    {-# INLINE toField #-}

instance FromField L.ByteString where
    parseField = pure . fromStrict
    {-# INLINE parseField #-}

instance ToField L.ByteString where
    toField = toStrict
    {-# INLINE toField #-}

#if MIN_VERSION_bytestring(0,10,4)
instance FromField SBS.ShortByteString where
    parseField = pure . SBS.toShort
    {-# INLINE parseField #-}

instance ToField SBS.ShortByteString where
    toField = SBS.fromShort
    {-# INLINE toField #-}
#endif

#if MIN_VERSION_text_short(0,1,0)
-- | Assumes UTF-8 encoding. Fails on invalid byte sequences.
--
-- @since 0.5.0.0
instance FromField T.S.ShortText where
    parseField = maybe (fail "Invalid UTF-8 stream") pure . T.S.fromByteString
    {-# INLINE parseField #-}

-- | Uses UTF-8 encoding.
--
-- @since 0.5.0.0
instance ToField T.S.ShortText where
    toField = T.S.toByteString
    {-# INLINE toField #-}
#endif

-- | Assumes UTF-8 encoding. Fails on invalid byte sequences.
instance FromField T.Text where
    parseField = either (fail . show) pure . T.decodeUtf8'
    {-# INLINE parseField #-}

-- | Uses UTF-8 encoding.
instance ToField T.Text where
    toField = toField . T.encodeUtf8
    {-# INLINE toField #-}

-- | Assumes UTF-8 encoding. Fails on invalid byte sequences.
instance FromField LT.Text where
    parseField = either (fail . show) (pure . LT.fromStrict) . T.decodeUtf8'
    {-# INLINE parseField #-}

-- | Uses UTF-8 encoding.
instance ToField LT.Text where
    toField = toField . toStrict . LT.encodeUtf8
    {-# INLINE toField #-}

-- | Assumes UTF-8 encoding. Fails on invalid byte sequences.
instance FromField [Char] where
    parseField = fmap T.unpack . parseField
    {-# INLINE parseField #-}

-- | Uses UTF-8 encoding.
instance ToField [Char] where
    toField = toField . T.pack
    {-# INLINE toField #-}

parseSigned :: (Integral a, Num a) => String -> B.ByteString -> Parser a
parseSigned typ s = case parseOnly (ws *> A8.signed A8.decimal <* ws) s of
    Left err -> typeError typ s (Just err)
    Right n  -> pure n
{-# INLINE parseSigned #-}

parseUnsigned :: Integral a => String -> B.ByteString -> Parser a
parseUnsigned typ s = case parseOnly (ws *> A8.decimal <* ws) s of
    Left err -> typeError typ s (Just err)
    Right n  -> pure n
{-# INLINE parseUnsigned #-}

ws :: A8.Parser ()
ws = A8.skipWhile (\c -> c == ' ' || c == '\t')



------------------------------------------------------------------------
-- Custom version of attoparsec @parseOnly@ function which fails if
-- there is leftover content after parsing a field.
parseOnly :: A8.Parser a -> B.ByteString -> Either String a
parseOnly parser input = go (A8.parse parser input) where
  go (A8.Fail _ _ err) = Left err
  go (A8.Partial f)    = go2 (f B.empty)
  go (A8.Done leftover result)
    | B.null leftover = Right result
    | otherwise = Left ("incomplete field parse, leftover: "
                        ++ show (B.unpack leftover))

  go2 (A8.Fail _ _ err) = Left err
  go2 (A8.Partial _)    = error "parseOnly: impossible error!"
  go2 (A8.Done leftover result)
    | B.null leftover = Right result
    | otherwise = Left ("incomplete field parse, leftover: "
                        ++ show (B.unpack leftover))
{-# INLINE parseOnly #-}

typeError :: String -> B.ByteString -> Maybe String -> Parser a
typeError typ s mmsg =
    fail $ "expected " ++ typ ++ ", got " ++ show (B8.unpack s) ++ cause
  where
    cause = case mmsg of
        Just msg -> " (" ++ msg ++ ")"
        Nothing  -> ""

------------------------------------------------------------------------
-- Constructors and accessors

-- | Retrieve the /n/th field in the given record. The result is
-- 'empty' if the value cannot be converted to the desired type.
-- Raises an exception if the index is out of bounds.
--
-- 'index' is a simple convenience function that is equivalent to
-- @'parseField' (v '!' idx)@. If you're certain that the index is not
-- out of bounds, using 'unsafeIndex' is somewhat faster.
index :: FromField a => Record -> Int -> Parser a
index v idx = parseField (v ! idx)
{-# INLINE index #-}

-- | Alias for 'index'.
(.!) :: FromField a => Record -> Int -> Parser a
(.!) = index
{-# INLINE (.!) #-}
infixl 9 .!

-- | Like 'index' but without bounds checking.
unsafeIndex :: FromField a => Record -> Int -> Parser a
unsafeIndex v idx = parseField (V.unsafeIndex v idx)
{-# INLINE unsafeIndex #-}

-- | Retrieve a field in the given record by name.  The result is
-- 'empty' if the field is missing or if the value cannot be converted
-- to the desired type.
lookup :: FromField a => NamedRecord -> B.ByteString -> Parser a
lookup m name = maybe (fail err) parseField' $ HM.lookup name m
  where err = "no field named " ++ show (B8.unpack name)
        parseField' fld = case runParser (parseField fld) of
          Left e -> fail $ "in named field " ++ show (B8.unpack name) ++ ": " ++ e
          Right res -> pure res
{-# INLINE lookup #-}

-- | Alias for 'lookup'.
(.:) :: FromField a => NamedRecord -> B.ByteString -> Parser a
(.:) = lookup
{-# INLINE (.:) #-}

-- | Construct a pair from a name and a value.  For use with
-- 'namedRecord'.
namedField :: ToField a => B.ByteString -> a -> (B.ByteString, B.ByteString)
namedField name val = (name, toField val)
{-# INLINE namedField #-}

-- | Alias for 'namedField'.
(.=) :: ToField a => B.ByteString -> a -> (B.ByteString, B.ByteString)
(.=) = namedField
{-# INLINE (.=) #-}

-- | Construct a record from a list of 'B.ByteString's.  Use 'toField'
-- to convert values to 'B.ByteString's for use with 'record'.
record :: [B.ByteString] -> Record
record = V.fromList

-- | Construct a named record from a list of name-value 'B.ByteString'
-- pairs.  Use '.=' to construct such a pair from a name and a value.
namedRecord :: [(B.ByteString, B.ByteString)] -> NamedRecord
namedRecord = HM.fromList

-- | Construct a header from a list of 'B.ByteString's.
header :: [B.ByteString] -> Header
header = V.fromList

------------------------------------------------------------------------
-- Parser for converting records to data types

-- | Failure continuation.
type Failure f r   = String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | Conversion of a field to a value might fail e.g. if the field is
-- malformed. This possibility is captured by the 'Parser' type, which
-- lets you compose several field conversions together in such a way
-- that if any of them fail, the whole record conversion fails.
newtype Parser a = Parser {
      unParser :: forall (f :: * -> *) (r :: *).
                  Failure f r
               -> Success a f r
               -> f r
    }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = unParser (g a) kf ks
                                 in unParser m kf ks'
    {-# INLINE (>>=) #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    return = pure
    {-# INLINE return #-}

#if !MIN_VERSION_base(4,13,0)
    fail = Fail.fail
    {-# INLINE fail #-}
#endif

-- | @since 0.5.0.0
instance Fail.MonadFail Parser where
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in unParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure a = Parser $ \_kf ks -> ks a
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = unParser b kf ks
                                   in unParser a kf' ks
    {-# INLINE mplus #-}

-- | @since 0.5.0.0
instance Semi.Semigroup (Parser a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (Semi.<>)
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  pure (b a)
{-# INLINE apP #-}

-- | Run a 'Parser', returning either @'Left' errMsg@ or @'Right'
-- result@. Forces the value in the 'Left' or 'Right' constructors to
-- weak head normal form.
--
-- You most likely won't need to use this function directly, but it's
-- included for completeness.
runParser :: Parser a -> Either String a
runParser p = unParser p left right
  where
    left !errMsg = Left errMsg
    right !x = Right x
{-# INLINE runParser #-}

------------------------------------------------------------------------
-- Generics

class GFromRecord f where
    gparseRecord :: Options -> Record -> Parser (f p)

instance GFromRecordSum f Record => GFromRecord (M1 i n f) where
    gparseRecord opts v =
        case IM.lookup n (gparseRecordSum opts) of
            Nothing -> lengthMismatch n v
            Just p -> M1 <$> p v
      where
        n = V.length v

class GFromNamedRecord f where
    gparseNamedRecord :: Options -> NamedRecord -> Parser (f p)

instance GFromRecordSum f NamedRecord => GFromNamedRecord (M1 i n f) where
    gparseNamedRecord opts v =
        foldr (\f p -> p <|> M1 <$> f v) empty (IM.elems (gparseRecordSum opts))

class GFromRecordSum f r where
    gparseRecordSum :: Options -> IM.IntMap (r -> Parser (f p))

instance (GFromRecordSum a r, GFromRecordSum b r) => GFromRecordSum (a :+: b) r where
    gparseRecordSum opts =
        IM.unionWith (\a b r -> a r <|> b r)
            (fmap (L1 <$>) <$> gparseRecordSum opts)
            (fmap (R1 <$>) <$> gparseRecordSum opts)

instance GFromRecordProd f r => GFromRecordSum (M1 i n f) r where
    gparseRecordSum opts = IM.singleton n (fmap (M1 <$>) f)
      where
        (n, f) = gparseRecordProd opts 0

class GFromRecordProd f r where
    gparseRecordProd :: Options -> Int -> (Int, r -> Parser (f p))

instance GFromRecordProd U1 r where
    gparseRecordProd _ n = (n, const (pure U1))

instance (GFromRecordProd a r, GFromRecordProd b r) => GFromRecordProd (a :*: b) r where
    gparseRecordProd opts n0 = (n2, f)
      where
        f r = (:*:) <$> fa r <*> fb r
        (n1, fa) = gparseRecordProd opts n0
        (n2, fb) = gparseRecordProd opts n1

instance GFromRecordProd f Record => GFromRecordProd (M1 i n f) Record where
    gparseRecordProd opts n = fmap (M1 <$>) <$> gparseRecordProd opts n

instance FromField a => GFromRecordProd (K1 i a) Record where
    gparseRecordProd _ n = (n + 1, \v -> K1 <$> parseField (V.unsafeIndex v n))

data Proxy s (f :: * -> *) a = Proxy

instance (FromField a, Selector s) => GFromRecordProd (M1 S s (K1 i a)) NamedRecord where
    gparseRecordProd opts n = (n + 1, \v -> (M1 . K1) <$> v .: name)
      where
        name = T.encodeUtf8 (T.pack (fieldLabelModifier opts (selName (Proxy :: Proxy s f a))))


class GToRecord a f where
    gtoRecord :: Options -> a p -> [f]

instance GToRecord U1 f where
    gtoRecord _ U1 = []

instance (GToRecord a f, GToRecord b f) => GToRecord (a :*: b) f where
    gtoRecord opts (a :*: b) = gtoRecord opts a ++ gtoRecord opts b

instance (GToRecord a f, GToRecord b f) => GToRecord (a :+: b) f where
    gtoRecord opts (L1 a) = gtoRecord opts a
    gtoRecord opts (R1 b) = gtoRecord opts b

instance GToRecord a f => GToRecord (M1 D c a) f where
    gtoRecord opts (M1 a) = gtoRecord opts a

instance GToRecord a f => GToRecord (M1 C c a) f where
    gtoRecord opts (M1 a) = gtoRecord opts a

instance GToRecord a Field => GToRecord (M1 S c a) Field where
    gtoRecord opts (M1 a) = gtoRecord opts a

instance ToField a => GToRecord (K1 i a) Field where
    gtoRecord _ (K1 a) = [toField a]

instance (ToField a, Selector s) => GToRecord (M1 S s (K1 i a)) (B.ByteString, B.ByteString) where
    gtoRecord opts m@(M1 (K1 a)) = [name .= toField a]
      where
        name = T.encodeUtf8 (T.pack (fieldLabelModifier opts (selName m)))

-- We statically fail on sum types and product types without selectors
-- (field names).

class GToNamedRecordHeader a
  where
    gtoNamedRecordHeader :: Options -> a p -> [Name]

instance GToNamedRecordHeader U1
  where
    gtoNamedRecordHeader _ _ = []

instance (GToNamedRecordHeader a, GToNamedRecordHeader b) =>
         GToNamedRecordHeader (a :*: b)
  where
    gtoNamedRecordHeader opts _ = gtoNamedRecordHeader opts (undefined :: a p) ++
                                  gtoNamedRecordHeader opts (undefined :: b p)

instance GToNamedRecordHeader a => GToNamedRecordHeader (M1 D c a)
  where
    gtoNamedRecordHeader opts _ = gtoNamedRecordHeader opts (undefined :: a p)

instance GToNamedRecordHeader a => GToNamedRecordHeader (M1 C c a)
  where
    gtoNamedRecordHeader opts _ = gtoNamedRecordHeader opts (undefined :: a p)

-- | Instance to ensure that you cannot derive DefaultOrdered for
-- constructors without selectors.
#if MIN_VERSION_base(4,9,0)
instance DefaultOrdered (M1 S ('MetaSel 'Nothing srcpk srcstr decstr) a ())
         => GToNamedRecordHeader (M1 S ('MetaSel 'Nothing srcpk srcstr decstr) a)
#else
instance DefaultOrdered (M1 S NoSelector a ()) => GToNamedRecordHeader (M1 S NoSelector a)
#endif
  where
    gtoNamedRecordHeader _ _ =
        error "You cannot derive DefaultOrdered for constructors without selectors."

instance Selector s => GToNamedRecordHeader (M1 S s a)
  where
    gtoNamedRecordHeader opts m
        | null name = error "Cannot derive DefaultOrdered for constructors without selectors"
        | otherwise = [B8.pack (fieldLabelModifier opts (selName m))]
      where name = selName m
