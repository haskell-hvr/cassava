{-# LANGUAGE 
    FlexibleInstances
  , FlexibleContexts
  , TypeOperators
  , MultiParamTypeClasses 
  , KindSignatures
  , ScopedTypeVariables
  , UndecidableInstances
  #-}
module Data.Csv.Conversion.Generics (

  -- $example
  
    parseRecord
  , parseNamedRecord
  , toRecord
  , toNamedRecord

  ) where

import GHC.Generics

import Control.Applicative
import qualified Data.Vector as V
import qualified Data.IntMap as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B

import Data.Csv.Types
import Data.Csv.Conversion hiding (parseRecord, parseNamedRecord, toRecord, toNamedRecord)
import qualified Data.Csv.Conversion as C


-- $example
--
-- With GHC Generics the instance methods for `C.FromRecord`, `C.FromNamedRecord`,
-- `C.ToRecord` and `C.ToNamedRecord` can be derived automatically.
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- > 
-- > import Data.Csv
-- > import qualified Data.Csv.Conversion.Generics as G
-- > import GHC.Generics
-- >
-- > data Person = Person
-- >     { name   :: String
-- >     , salary :: Int
-- >     }
-- >     deriving Generic

-- | Generic version of 'C.parseRecord'.
--
-- > instance FromRecord Person where
-- >     parseRecord = G.parseRecord
parseRecord :: (Generic a, GFromRecord (Rep a)) => Record -> Parser a
parseRecord r = to <$> gparseRecord r

class GFromRecord f where
    gparseRecord :: Record -> Parser (f p)

instance GFromRecordSum f Record => GFromRecord (M1 i n f) where
    gparseRecord v = 
        case (M.lookup n gparseRecordSum) of
            Nothing -> lengthMismatch n v 
            Just p -> M1 <$> p v
      where
        n = V.length v


-- | Generic version of 'C.parseNamedRecord'.
--
-- > instance FromNamedRecord Person where
-- >     parseNamedRecord = G.parseNamedRecord
parseNamedRecord :: (Generic a, GFromNamedRecord (Rep a)) => NamedRecord -> Parser a
parseNamedRecord r = to <$> gparseNamedRecord r

class GFromNamedRecord f where
    gparseNamedRecord :: NamedRecord -> Parser (f p)

instance GFromRecordSum f NamedRecord => GFromNamedRecord (M1 i n f) where
    gparseNamedRecord v = foldr (\f p -> M1 <$> f v <|> p) empty (M.elems gparseRecordSum)


class GFromRecordSum f r where
    gparseRecordSum :: M.IntMap (r -> Parser (f p))

instance (GFromRecordSum a r, GFromRecordSum b r) => GFromRecordSum (a :+: b) r where
    gparseRecordSum = 
        M.unionWith (\a b r -> a r <|> b r) 
            (fmap (L1 <$>) <$> gparseRecordSum)
            (fmap (R1 <$>) <$> gparseRecordSum)
  
instance GFromRecordProd f r => GFromRecordSum (M1 i n f) r where
    gparseRecordSum = M.singleton n (fmap (M1 <$>) f)
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
            

-- | Generic version of 'C.toRecord'.
--   
-- > instance ToRecord Person where
-- >   toRecord = G.toRecord
toRecord :: (Generic a, GToRecord (Rep a) Field) => a -> Record
toRecord = V.fromList . gtoRecord . from

-- | Generic version of 'C.toNamedRecord'.
--   
-- > instance ToNamedRecord Person where
-- >   toNamedRecord = G.toNamedRecord
toNamedRecord :: (Generic a, GToRecord (Rep a) (B.ByteString, B.ByteString)) => a -> NamedRecord
toNamedRecord = namedRecord . gtoRecord . from

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
