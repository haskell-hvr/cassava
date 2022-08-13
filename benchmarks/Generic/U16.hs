{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Generic.U16
  ( U16
  , U16Generic
  , U16GenericStripPrefix
  ) where

import Control.DeepSeq
import Data.Csv
import Data.Typeable
import Generic.Prefix
import GHC.Generics (Generic)


data U16
  = U16ManualXXXXXX01 | U16ManualXXXXXX02 | U16ManualXXXXXX03 | U16ManualXXXXXX04
  | U16ManualXXXXXX05 | U16ManualXXXXXX06 | U16ManualXXXXXX07 | U16ManualXXXXXX08
  | U16ManualXXXXXX09 | U16ManualXXXXXX10 | U16ManualXXXXXX11 | U16ManualXXXXXX12
  | U16ManualXXXXXX13 | U16ManualXXXXXX14 | U16ManualXXXXXX15 | U16ManualXXXXXX16
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U16 where
  parseField s = case s of
    "XXXXXX01" -> pure U16ManualXXXXXX01
    "XXXXXX02" -> pure U16ManualXXXXXX02
    "XXXXXX03" -> pure U16ManualXXXXXX03
    "XXXXXX04" -> pure U16ManualXXXXXX04
    "XXXXXX05" -> pure U16ManualXXXXXX05
    "XXXXXX06" -> pure U16ManualXXXXXX06
    "XXXXXX07" -> pure U16ManualXXXXXX07
    "XXXXXX08" -> pure U16ManualXXXXXX08
    "XXXXXX09" -> pure U16ManualXXXXXX09
    "XXXXXX10" -> pure U16ManualXXXXXX10
    "XXXXXX11" -> pure U16ManualXXXXXX11
    "XXXXXX12" -> pure U16ManualXXXXXX12
    "XXXXXX13" -> pure U16ManualXXXXXX13
    "XXXXXX14" -> pure U16ManualXXXXXX14
    "XXXXXX15" -> pure U16ManualXXXXXX15
    "XXXXXX16" -> pure U16ManualXXXXXX16
    _ -> fail "No parse"

instance ToField U16 where
  toField x = case x of
    U16ManualXXXXXX01 -> "XXXXXX01"
    U16ManualXXXXXX02 -> "XXXXXX02"
    U16ManualXXXXXX03 -> "XXXXXX03"
    U16ManualXXXXXX04 -> "XXXXXX04"
    U16ManualXXXXXX05 -> "XXXXXX05"
    U16ManualXXXXXX06 -> "XXXXXX06"
    U16ManualXXXXXX07 -> "XXXXXX07"
    U16ManualXXXXXX08 -> "XXXXXX08"
    U16ManualXXXXXX09 -> "XXXXXX09"
    U16ManualXXXXXX10 -> "XXXXXX10"
    U16ManualXXXXXX11 -> "XXXXXX11"
    U16ManualXXXXXX12 -> "XXXXXX12"
    U16ManualXXXXXX13 -> "XXXXXX13"
    U16ManualXXXXXX14 -> "XXXXXX14"
    U16ManualXXXXXX15 -> "XXXXXX15"
    U16ManualXXXXXX16 -> "XXXXXX16"

data U16Generic
  = XXXXXX01 | XXXXXX02 | XXXXXX03 | XXXXXX04
  | XXXXXX05 | XXXXXX06 | XXXXXX07 | XXXXXX08
  | XXXXXX09 | XXXXXX10 | XXXXXX11 | XXXXXX12
  | XXXXXX13 | XXXXXX14 | XXXXXX15 | XXXXXX16
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U16Generic

instance ToField U16Generic

data U16GenericStripPrefix
  = U16XXXXXX01 | U16XXXXXX02 | U16XXXXXX03 | U16XXXXXX04
  | U16XXXXXX05 | U16XXXXXX06 | U16XXXXXX07 | U16XXXXXX08
  | U16XXXXXX09 | U16XXXXXX10 | U16XXXXXX11 | U16XXXXXX12
  | U16XXXXXX13 | U16XXXXXX14 | U16XXXXXX15 | U16XXXXXX16
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U16GenericStripPrefix where
  parseField = genericParseField defaultOptions{fieldLabelModifier = dropPrefix "U16"}

instance ToField U16GenericStripPrefix where
  toField = genericToField defaultOptions{fieldLabelModifier = dropPrefix "U16"}
