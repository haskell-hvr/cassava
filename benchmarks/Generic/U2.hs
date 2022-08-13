{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Generic.U2
  ( U2
  , U2Generic
  , U2GenericStripPrefix
  ) where

import Control.DeepSeq
import Data.Csv
import Data.Typeable
import Generic.Prefix
import GHC.Generics (Generic)


data U2
  = U2ManualXXXXXX01 | U2ManualXXXXXX02
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U2 where
  parseField s = case s of
    "XXXXXX01" -> pure U2ManualXXXXXX01
    "XXXXXX02" -> pure U2ManualXXXXXX02
    _ -> fail "No parse"

instance ToField U2 where
  toField x = case x of
    U2ManualXXXXXX01 -> "XXXXXX01"
    U2ManualXXXXXX02 -> "XXXXXX02"

data U2Generic
  = XXXXXX01 | XXXXXX02
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U2Generic

instance ToField U2Generic

data U2GenericStripPrefix
  = U2XXXXXX01 | U2XXXXXX02
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U2GenericStripPrefix where
  parseField = genericParseField defaultOptions{fieldLabelModifier = dropPrefix "U2"}

instance ToField U2GenericStripPrefix where
  toField = genericToField defaultOptions{fieldLabelModifier = dropPrefix "U2"}
