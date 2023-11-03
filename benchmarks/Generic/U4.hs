{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Generic.U4
  ( U4
  , U4Generic
  , U4GenericStripPrefix
  ) where

import Control.DeepSeq
import Data.Csv
import Data.Typeable
import Generic.Prefix
import GHC.Generics (Generic)


data U4
  = U4ManualXXXXXX01 | U4ManualXXXXXX02 | U4ManualXXXXXX03 | U4ManualXXXXXX04
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U4 where
  parseField s = case s of
    "XXXXXX01" -> pure U4ManualXXXXXX01
    "XXXXXX02" -> pure U4ManualXXXXXX02
    "XXXXXX03" -> pure U4ManualXXXXXX03
    "XXXXXX04" -> pure U4ManualXXXXXX04
    _ -> fail "No parse"

instance ToField U4 where
  toField x = case x of
    U4ManualXXXXXX01 -> "XXXXXX01"
    U4ManualXXXXXX02 -> "XXXXXX02"
    U4ManualXXXXXX03 -> "XXXXXX03"
    U4ManualXXXXXX04 -> "XXXXXX04"

data U4Generic
  = XXXXXX01 | XXXXXX02 | XXXXXX03 | XXXXXX04
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U4Generic

instance ToField U4Generic

data U4GenericStripPrefix
  = U4XXXXXX01 | U4XXXXXX02 | U4XXXXXX03 | U4XXXXXX04
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U4GenericStripPrefix where
  parseField = genericParseField defaultOptions{fieldLabelModifier = dropPrefix "U4"}

instance ToField U4GenericStripPrefix where
  toField = genericToField defaultOptions{fieldLabelModifier = dropPrefix "U4"}
