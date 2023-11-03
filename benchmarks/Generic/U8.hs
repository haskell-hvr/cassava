{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Generic.U8
  ( U8
  , U8Generic
  , U8GenericStripPrefix
  ) where

import Control.DeepSeq
import Data.Csv
import Data.Typeable
import Generic.Prefix
import GHC.Generics (Generic)


data U8
  = U8ManualXXXXXX01 | U8ManualXXXXXX02 | U8ManualXXXXXX03 | U8ManualXXXXXX04
  | U8ManualXXXXXX05 | U8ManualXXXXXX06 | U8ManualXXXXXX07 | U8ManualXXXXXX08
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U8 where
  parseField s = case s of
    "XXXXXX01" -> pure U8ManualXXXXXX01
    "XXXXXX02" -> pure U8ManualXXXXXX02
    "XXXXXX03" -> pure U8ManualXXXXXX03
    "XXXXXX04" -> pure U8ManualXXXXXX04
    "XXXXXX05" -> pure U8ManualXXXXXX05
    "XXXXXX06" -> pure U8ManualXXXXXX06
    "XXXXXX07" -> pure U8ManualXXXXXX07
    "XXXXXX08" -> pure U8ManualXXXXXX08
    _ -> fail "No parse"

instance ToField U8 where
  toField x = case x of
    U8ManualXXXXXX01 -> "XXXXXX01"
    U8ManualXXXXXX02 -> "XXXXXX02"
    U8ManualXXXXXX03 -> "XXXXXX03"
    U8ManualXXXXXX04 -> "XXXXXX04"
    U8ManualXXXXXX05 -> "XXXXXX05"
    U8ManualXXXXXX06 -> "XXXXXX06"
    U8ManualXXXXXX07 -> "XXXXXX07"
    U8ManualXXXXXX08 -> "XXXXXX08"

data U8Generic
  = XXXXXX01 | XXXXXX02 | XXXXXX03 | XXXXXX04
  | XXXXXX05 | XXXXXX06 | XXXXXX07 | XXXXXX08
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U8Generic

instance ToField U8Generic

data U8GenericStripPrefix
  = U8XXXXXX01 | U8XXXXXX02 | U8XXXXXX03 | U8XXXXXX04
  | U8XXXXXX05 | U8XXXXXX06 | U8XXXXXX07 | U8XXXXXX08
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U8GenericStripPrefix where
  parseField = genericParseField defaultOptions{fieldLabelModifier = dropPrefix "U8"}

instance ToField U8GenericStripPrefix where
  toField = genericToField defaultOptions{fieldLabelModifier = dropPrefix "U8"}
