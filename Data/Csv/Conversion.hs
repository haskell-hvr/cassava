{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, Rank2Types #-}
#ifdef GENERICS
{-# LANGUAGE DefaultSignatures, TypeOperators, KindSignatures, FlexibleContexts,
             MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables #-}
#endif
module Data.Csv.Conversion
    (
    -- * Type conversion
      Only(..)
    , FromRecord(..)
    , FromNamedRecord(..)
    , ToNamedRecord(..)
    , FromField(..)
    , ToRecord(..)
    , ToField(..)

    -- * Parser
    , Result(..)
    , Parser
    , parse
    , parseEither

    -- * Accessors
    , (.!)
    , (.:)
    , (.=)
    , record
    , namedRecord
    ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Char8 (double, number, parseOnly)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Lazy as HM
import Data.Int
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Traversable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word
import GHC.Float (double2Float)
import Prelude hiding (takeWhile)

import Data.Csv.Conversion.Internal
import Data.Csv.Types

#ifdef GENERICS
import GHC.Generics
import qualified Data.IntMap as IM
#endif

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
  
#ifdef GENERICS
    default parseRecord :: (Generic a, GFromRecord (Rep a)) => Record -> Parser a
    parseRecord r = to <$> gparseRecord r
#endif

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
--     toRecord (Person name age) = 'record' [
--        'toField' name, 'toField' age]
-- @
--
-- Outputs data on this form:
--
-- > John,56
-- > Jane,55
class ToRecord a where
    toRecord :: a -> Record

#ifdef GENERICS
    default toRecord :: (Generic a, GToRecord (Rep a) Field) => a -> Record
    toRecord = V.fromList . gtoRecord . from
#endif

instance FromField a => FromRecord (Only a) where
    parseRecord v
        | n == 1    = Only <$> parseField (V.unsafeIndex v 0)
        | otherwise = lengthMismatch 1 v
          where
            n = V.length v

-- TODO: Check if we want all toRecord conversions to be stricter.

instance ToField a => ToRecord (Only a) where
    toRecord = V.singleton . toField . fromOnly

instance (FromField a, FromField b) => FromRecord (a, b) where
    parseRecord v
        | n == 2    = (,) <$> parseField (V.unsafeIndex v 0)
                          <*> parseField (V.unsafeIndex v 1)
        | otherwise = lengthMismatch 2 v
          where
            n = V.length v

instance (ToField a, ToField b) => ToRecord (a, b) where
    toRecord (a, b) = V.fromList [toField a, toField b]

instance (FromField a, FromField b, FromField c) => FromRecord (a, b, c) where
    parseRecord v
        | n == 3    = (,,) <$> parseField (V.unsafeIndex v 0)
                           <*> parseField (V.unsafeIndex v 1)
                           <*> parseField (V.unsafeIndex v 2)
        | otherwise = lengthMismatch 3 v
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
        | n == 5    = (,,,,) <$> parseField (V.unsafeIndex v 0)
                             <*> parseField (V.unsafeIndex v 1)
                             <*> parseField (V.unsafeIndex v 2)
                             <*> parseField (V.unsafeIndex v 3)
                             <*> parseField (V.unsafeIndex v 4)
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
        | n == 6    = (,,,,,) <$> parseField (V.unsafeIndex v 0)
                              <*> parseField (V.unsafeIndex v 1)
                              <*> parseField (V.unsafeIndex v 2)
                              <*> parseField (V.unsafeIndex v 3)
                              <*> parseField (V.unsafeIndex v 4)
                              <*> parseField (V.unsafeIndex v 5)
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
        | n == 7    = (,,,,,,) <$> parseField (V.unsafeIndex v 0)
                               <*> parseField (V.unsafeIndex v 1)
                               <*> parseField (V.unsafeIndex v 2)
                               <*> parseField (V.unsafeIndex v 3)
                               <*> parseField (V.unsafeIndex v 4)
                               <*> parseField (V.unsafeIndex v 5)
                               <*> parseField (V.unsafeIndex v 6)
        | otherwise = lengthMismatch 7 v
          where
            n = V.length v

instance (ToField a, ToField b, ToField c, ToField d, ToField e, ToField f,
          ToField g) =>
         ToRecord (a, b, c, d, e, f, g) where
    toRecord (a, b, c, d, e, f, g) = V.fromList [
        toField a, toField b, toField c, toField d, toField e, toField f,
        toField g]

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

#ifdef GENERICS
    default parseNamedRecord :: (Generic a, GFromNamedRecord (Rep a)) => NamedRecord -> Parser a
    parseNamedRecord r = to <$> gparseNamedRecord r
#endif

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

#ifdef GENERICS
    default toNamedRecord :: (Generic a, GToRecord (Rep a) (B.ByteString, B.ByteString)) => a -> NamedRecord
    toNamedRecord = namedRecord . gtoRecord . from
#endif

instance FromField a => FromNamedRecord (M.Map B.ByteString a) where
    parseNamedRecord m = M.fromList <$>
                         (traverse parseSnd $ HM.toList m)
      where parseSnd (name, s) = (,) <$> pure name <*> parseField s

instance ToField a => ToNamedRecord (M.Map B.ByteString a) where
    toNamedRecord = HM.fromList . map (\ (k, v) -> (k, toField v)) . M.toList

instance FromField a => FromNamedRecord (HM.HashMap B.ByteString a) where
    parseNamedRecord m = traverse (\ s -> parseField s) m

instance ToField a => ToNamedRecord (HM.HashMap B.ByteString a) where
    toNamedRecord = HM.map toField

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

-- | A type that can be converted to a single CSV field.
--
-- Example type and instance:
--
-- @{-\# LANGUAGE OverloadedStrings \#-}
--
-- data Color = Red | Green | Blue
--
-- instance ToField Color where
--     toField Red   = \"R\"
--     toField Green = \"G\"
--     toField Blue  = \"B\"
-- @
class ToField a where
    toField :: a -> Field

instance FromField a => FromField (Maybe a) where
    parseField s
        | B.null s  = pure Nothing
        | otherwise = Just <$> parseField s
    {-# INLINE parseField #-}

instance ToField a => ToField (Maybe a) where
    toField = maybe B.empty toField
    {-# INLINE toField #-}

instance FromField Char where
    parseField s
        | T.compareLength t 1 == EQ = pure (T.head t)
        | otherwise = typeError "Char" s Nothing
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
    Left err -> typeError "Double" s (Just err)
    Right n  -> pure n
{-# INLINE parseDouble #-}

instance FromField Int where
    parseField = parseIntegral "Int"
    {-# INLINE parseField #-}

instance ToField Int where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Integer where
    parseField = parseIntegral "Integer"
    {-# INLINE parseField #-}

instance ToField Integer where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Int8 where
    parseField = parseIntegral "Int8"
    {-# INLINE parseField #-}

instance ToField Int8 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Int16 where
    parseField = parseIntegral "Int16"
    {-# INLINE parseField #-}

instance ToField Int16 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Int32 where
    parseField = parseIntegral "Int32"
    {-# INLINE parseField #-}

instance ToField Int32 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Int64 where
    parseField = parseIntegral "Int64"
    {-# INLINE parseField #-}

instance ToField Int64 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word where
    parseField = parseIntegral "Word"
    {-# INLINE parseField #-}

instance ToField Word where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word8 where
    parseField = parseIntegral "Word8"
    {-# INLINE parseField #-}

instance ToField Word8 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word16 where
    parseField = parseIntegral "Word16"
    {-# INLINE parseField #-}

instance ToField Word16 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word32 where
    parseField = parseIntegral "Word32"
    {-# INLINE parseField #-}

instance ToField Word32 where
    toField = decimal
    {-# INLINE toField #-}

instance FromField Word64 where
    parseField = parseIntegral "Word64"
    {-# INLINE parseField #-}

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
    parseField s = pure (L.fromChunks [s])
    {-# INLINE parseField #-}

instance ToField L.ByteString where
    toField = toField . B.concat . L.toChunks
    {-# INLINE toField #-}

-- | Assumes UTF-8 encoding.
instance FromField T.Text where
    parseField = pure . T.decodeUtf8
    {-# INLINE parseField #-}

-- | Uses UTF-8 encoding.
instance ToField T.Text where
    toField = toField . T.encodeUtf8
    {-# INLINE toField #-}

-- | Assumes UTF-8 encoding.
instance FromField LT.Text where
    parseField s = pure (LT.fromChunks [T.decodeUtf8 s])
    {-# INLINE parseField #-}

-- | Uses UTF-8 encoding.
instance ToField LT.Text where
    toField = toField . B.concat . L.toChunks . LT.encodeUtf8
    {-# INLINE toField #-}

-- | Assumes UTF-8 encoding.
instance FromField [Char] where
    parseField = fmap T.unpack . parseField
    {-# INLINE parseField #-}

-- | Uses UTF-8 encoding.
instance ToField [Char] where
    toField = toField . T.pack
    {-# INLINE toField #-}

parseIntegral :: Integral a => String -> B.ByteString -> Parser a
parseIntegral typ s = case parseOnly number s of
    Left err -> typeError typ s (Just err)
    Right n  -> pure (floor n)
{-# INLINE parseIntegral #-}

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
(.!) :: FromField a => Record -> Int -> Parser a
v .! idx = parseField (v ! idx)
{-# INLINE (.!) #-}

-- | Retrieve a field in the given record by name.  The result is
-- 'empty' if the field is missing or if the value cannot be converted
-- to the desired type.
(.:) :: FromField a => NamedRecord -> B.ByteString -> Parser a
m .: name = maybe (fail err) parseField $ HM.lookup name m
  where err = "no field named " ++ show (B8.unpack name)
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

------------------------------------------------------------------------
-- Parser for converting records to data types

-- | The result of running a 'Parser'.
data Result a = Error String
              | Success a
                deriving (Eq, Show)

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad Result where
    return = Success
    {-# INLINE return #-}
    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

-- | Failure continuation.
type Failure f r   = String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | Conversion of a field to a value might fail e.g. if the field is
-- malformed. This possibility is captured by the 'Parser' type, which
-- lets you compose several field conversions together in such a way
-- that if any of them fail, the whole record conversion fails.
newtype Parser a = Parser {
      runParser :: forall f r.
                   Failure f r
                -> Success a f r
                -> f r
    }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
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
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                   in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

-- | Run a 'Parser'.
parse :: Parser a -> Result a
parse p = runParser p Error Success
{-# INLINE parse #-}

-- | Run a 'Parser', returning either @'Left' msg@ or @'Right'
-- result@.
parseEither :: Parser a -> Either String a
parseEither p = runParser p Left Right
{-# INLINE parseEither #-}

#ifdef GENERICS

class GFromRecord f where
    gparseRecord :: Record -> Parser (f p)

instance GFromRecordSum f Record => GFromRecord (M1 i n f) where
    gparseRecord v = 
        case (IM.lookup n gparseRecordSum) of
            Nothing -> lengthMismatch n v 
            Just p -> M1 <$> p v
      where
        n = V.length v

class GFromNamedRecord f where
    gparseNamedRecord :: NamedRecord -> Parser (f p)

instance GFromRecordSum f NamedRecord => GFromNamedRecord (M1 i n f) where
    gparseNamedRecord v = 
        foldr (\f p -> p <|> M1 <$> f v) empty (IM.elems gparseRecordSum)


class GFromRecordSum f r where
    gparseRecordSum :: IM.IntMap (r -> Parser (f p))

instance (GFromRecordSum a r, GFromRecordSum b r) => GFromRecordSum (a :+: b) r where
    gparseRecordSum = 
        IM.unionWith (\a b r -> a r <|> b r) 
            (fmap (L1 <$>) <$> gparseRecordSum)
            (fmap (R1 <$>) <$> gparseRecordSum)

instance GFromRecordProd f r => GFromRecordSum (M1 i n f) r where
    gparseRecordSum = IM.singleton n (fmap (M1 <$>) f)
      where
        (n, f) = gparseRecordProd 0

class GFromRecordProd f r where
    gparseRecordProd :: Int -> (Int, r -> Parser (f p))

instance GFromRecordProd U1 r where
    gparseRecordProd n = (n, const (pure U1))

instance (GFromRecordProd a r, GFromRecordProd b r) => GFromRecordProd (a :*: b) r where
    gparseRecordProd n0 = (n2, f)
      where
        f r = (:*:) <$> fa r <*> fb r
        (n1, fa) = gparseRecordProd n0
        (n2, fb) = gparseRecordProd n1

instance GFromRecordProd f Record => GFromRecordProd (M1 i n f) Record where
    gparseRecordProd n = fmap (M1 <$>) <$> gparseRecordProd n

instance FromField a => GFromRecordProd (K1 i a) Record where
    gparseRecordProd n = (n + 1, \v -> K1 <$> parseField (V.unsafeIndex v n))

data Proxy s (f :: * -> *) a = Proxy

instance (FromField a, Selector s) => GFromRecordProd (M1 S s (K1 i a)) NamedRecord where
    gparseRecordProd n = (n + 1, \v -> (M1 . K1) <$> v .: name)
      where
        name = T.encodeUtf8 (T.pack (selName (Proxy :: Proxy s f a)))


class GToRecord a f where
    gtoRecord :: a p -> [f]

instance GToRecord U1 f where
    gtoRecord U1 = []

instance (GToRecord a f, GToRecord b f) => GToRecord (a :*: b) f where
    gtoRecord (a :*: b) = gtoRecord a ++ gtoRecord b

instance (GToRecord a f, GToRecord b f) => GToRecord (a :+: b) f where
    gtoRecord (L1 a) = gtoRecord a
    gtoRecord (R1 b) = gtoRecord b

instance GToRecord a f => GToRecord (M1 D c a) f where
    gtoRecord (M1 a) = gtoRecord a

instance GToRecord a f => GToRecord (M1 C c a) f where
    gtoRecord (M1 a) = gtoRecord a

instance GToRecord a Field => GToRecord (M1 S c a) Field where
    gtoRecord (M1 a) = gtoRecord a

instance ToField a => GToRecord (K1 i a) Field where
    gtoRecord (K1 a) = [toField a]

instance (ToField a, Selector s) => GToRecord (M1 S s (K1 i a)) (B.ByteString, B.ByteString) where
    gtoRecord m@(M1 (K1 a)) = [T.encodeUtf8 (T.pack (selName m)) .= toField a]

#endif