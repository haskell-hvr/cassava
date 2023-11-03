{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Generic.Either
  ( EitherManual(..)
  , ManualEither0
  , ManualEither1
  , ManualEither2
  , ManualEither3
  , EitherGeneric(..)
  , GenericEither0
  , GenericEither1
  , GenericEither2
  , GenericEither3
  ) where

import Control.DeepSeq
import Data.Csv
import Data.Proxy
import Data.Typeable
import GHC.Generics (Generic)


data EitherManual a b = LManual a | RManual b
  deriving (Generic, NFData, Show, Typeable)

instance (FromField a, FromField b, Typeable a, Typeable b) => FromField (EitherManual a b) where
  parseField field = case runParser (parseField field) of
    Left _ -> case runParser (parseField field) of
      Left _ -> fail $ "Can't parse field of type "
        <> show (typeRep $ Proxy @(EitherManual a b)) <> " from " <> show field
      Right ok -> pure $ RManual ok
    Right ok -> pure $ LManual ok

instance (ToField a, ToField b) => ToField (EitherManual a b) where
  toField (LManual x) = toField x
  toField (RManual x) = toField x

data EitherGeneric a b = LGeneric a | RGeneric b
  deriving (Generic, NFData, Show, Typeable)

instance (FromField a, FromField b) => FromField (EitherGeneric a b)
instance (ToField a, ToField b) => ToField (EitherGeneric a b)

type Either0  f = f Int Char
type Either1  f = f (Either0 f) (Either0 f)
type Either2  f = f (Either1 f) (Either1 f)
type Either3  f = f (Either2 f) (Either2 f)
type Either4  f = f (Either3 f) (Either3 f)
type Either5  f = f (Either4 f) (Either4 f)
type Either6  f = f (Either5 f) (Either5 f)
type Either7  f = f (Either6 f) (Either6 f)
type Either8  f = f (Either7 f) (Either7 f)
type Either9  f = f (Either8 f) (Either8 f)
type Either10 f = f (Either9 f) (Either9 f)
type Either11 f = f (Either10 f) (Either10 f)
type Either12 f = f (Either11 f) (Either11 f)
type Either13 f = f (Either12 f) (Either12 f)
type Either14 f = f (Either13 f) (Either13 f)
type Either15 f = f (Either14 f) (Either14 f)
type Either16 f = f (Either15 f) (Either15 f)

type ManualEither0 = Either0 EitherManual
type ManualEither1 = Either1 EitherManual
type ManualEither2 = Either2 EitherManual
type ManualEither3 = Either3 EitherManual

type GenericEither0 = Either0 EitherGeneric
type GenericEither1 = Either1 EitherGeneric
type GenericEither2 = Either2 EitherGeneric
type GenericEither3 = Either3 EitherGeneric
