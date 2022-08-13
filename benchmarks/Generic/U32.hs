{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Generic.U32
  ( U32
  , U32Generic
  , U32GenericStripPrefix
  ) where

import Control.DeepSeq
import Data.Csv
import Data.Typeable
import Generic.Prefix
import GHC.Generics (Generic)


data U32
  = U32ManualXXXXXX01 | U32ManualXXXXXX02 | U32ManualXXXXXX03 | U32ManualXXXXXX04
  | U32ManualXXXXXX05 | U32ManualXXXXXX06 | U32ManualXXXXXX07 | U32ManualXXXXXX08
  | U32ManualXXXXXX09 | U32ManualXXXXXX10 | U32ManualXXXXXX11 | U32ManualXXXXXX12
  | U32ManualXXXXXX13 | U32ManualXXXXXX14 | U32ManualXXXXXX15 | U32ManualXXXXXX16
  | U32ManualXXXXXX17 | U32ManualXXXXXX18 | U32ManualXXXXXX19 | U32ManualXXXXXX20
  | U32ManualXXXXXX21 | U32ManualXXXXXX22 | U32ManualXXXXXX23 | U32ManualXXXXXX24
  | U32ManualXXXXXX25 | U32ManualXXXXXX26 | U32ManualXXXXXX27 | U32ManualXXXXXX28
  | U32ManualXXXXXX29 | U32ManualXXXXXX30 | U32ManualXXXXXX31 | U32ManualXXXXXX32
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U32 where
  parseField s = case s of
    "XXXXXX01" -> pure U32ManualXXXXXX01
    "XXXXXX02" -> pure U32ManualXXXXXX02
    "XXXXXX03" -> pure U32ManualXXXXXX03
    "XXXXXX04" -> pure U32ManualXXXXXX04
    "XXXXXX05" -> pure U32ManualXXXXXX05
    "XXXXXX06" -> pure U32ManualXXXXXX06
    "XXXXXX07" -> pure U32ManualXXXXXX07
    "XXXXXX08" -> pure U32ManualXXXXXX08
    "XXXXXX09" -> pure U32ManualXXXXXX09
    "XXXXXX10" -> pure U32ManualXXXXXX10
    "XXXXXX11" -> pure U32ManualXXXXXX11
    "XXXXXX12" -> pure U32ManualXXXXXX12
    "XXXXXX13" -> pure U32ManualXXXXXX13
    "XXXXXX14" -> pure U32ManualXXXXXX14
    "XXXXXX15" -> pure U32ManualXXXXXX15
    "XXXXXX16" -> pure U32ManualXXXXXX16
    "XXXXXX17" -> pure U32ManualXXXXXX17
    "XXXXXX18" -> pure U32ManualXXXXXX18
    "XXXXXX19" -> pure U32ManualXXXXXX19
    "XXXXXX20" -> pure U32ManualXXXXXX20
    "XXXXXX21" -> pure U32ManualXXXXXX21
    "XXXXXX22" -> pure U32ManualXXXXXX22
    "XXXXXX23" -> pure U32ManualXXXXXX23
    "XXXXXX24" -> pure U32ManualXXXXXX24
    "XXXXXX25" -> pure U32ManualXXXXXX25
    "XXXXXX26" -> pure U32ManualXXXXXX26
    "XXXXXX27" -> pure U32ManualXXXXXX27
    "XXXXXX28" -> pure U32ManualXXXXXX28
    "XXXXXX29" -> pure U32ManualXXXXXX29
    "XXXXXX30" -> pure U32ManualXXXXXX30
    "XXXXXX31" -> pure U32ManualXXXXXX31
    "XXXXXX32" -> pure U32ManualXXXXXX32
    _ -> fail "No parse"

instance ToField U32 where
  toField x = case x of
    U32ManualXXXXXX01 -> "XXXXXX01"
    U32ManualXXXXXX02 -> "XXXXXX02"
    U32ManualXXXXXX03 -> "XXXXXX03"
    U32ManualXXXXXX04 -> "XXXXXX04"
    U32ManualXXXXXX05 -> "XXXXXX05"
    U32ManualXXXXXX06 -> "XXXXXX06"
    U32ManualXXXXXX07 -> "XXXXXX07"
    U32ManualXXXXXX08 -> "XXXXXX08"
    U32ManualXXXXXX09 -> "XXXXXX09"
    U32ManualXXXXXX10 -> "XXXXXX10"
    U32ManualXXXXXX11 -> "XXXXXX11"
    U32ManualXXXXXX12 -> "XXXXXX12"
    U32ManualXXXXXX13 -> "XXXXXX13"
    U32ManualXXXXXX14 -> "XXXXXX14"
    U32ManualXXXXXX15 -> "XXXXXX15"
    U32ManualXXXXXX16 -> "XXXXXX16"
    U32ManualXXXXXX17 -> "XXXXXX17"
    U32ManualXXXXXX18 -> "XXXXXX18"
    U32ManualXXXXXX19 -> "XXXXXX19"
    U32ManualXXXXXX20 -> "XXXXXX20"
    U32ManualXXXXXX21 -> "XXXXXX21"
    U32ManualXXXXXX22 -> "XXXXXX22"
    U32ManualXXXXXX23 -> "XXXXXX23"
    U32ManualXXXXXX24 -> "XXXXXX24"
    U32ManualXXXXXX25 -> "XXXXXX25"
    U32ManualXXXXXX26 -> "XXXXXX26"
    U32ManualXXXXXX27 -> "XXXXXX27"
    U32ManualXXXXXX28 -> "XXXXXX28"
    U32ManualXXXXXX29 -> "XXXXXX29"
    U32ManualXXXXXX30 -> "XXXXXX30"
    U32ManualXXXXXX31 -> "XXXXXX31"
    U32ManualXXXXXX32 -> "XXXXXX32"

data U32Generic
  = XXXXXX01 | XXXXXX02 | XXXXXX03 | XXXXXX04
  | XXXXXX05 | XXXXXX06 | XXXXXX07 | XXXXXX08
  | XXXXXX09 | XXXXXX10 | XXXXXX11 | XXXXXX12
  | XXXXXX13 | XXXXXX14 | XXXXXX15 | XXXXXX16
  | XXXXXX17 | XXXXXX18 | XXXXXX19 | XXXXXX20
  | XXXXXX21 | XXXXXX22 | XXXXXX23 | XXXXXX24
  | XXXXXX25 | XXXXXX26 | XXXXXX27 | XXXXXX28
  | XXXXXX29 | XXXXXX30 | XXXXXX31 | XXXXXX32
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U32Generic

instance ToField U32Generic

data U32GenericStripPrefix
  = U32XXXXXX01 | U32XXXXXX02 | U32XXXXXX03 | U32XXXXXX04
  | U32XXXXXX05 | U32XXXXXX06 | U32XXXXXX07 | U32XXXXXX08
  | U32XXXXXX09 | U32XXXXXX10 | U32XXXXXX11 | U32XXXXXX12
  | U32XXXXXX13 | U32XXXXXX14 | U32XXXXXX15 | U32XXXXXX16
  | U32XXXXXX17 | U32XXXXXX18 | U32XXXXXX19 | U32XXXXXX20
  | U32XXXXXX21 | U32XXXXXX22 | U32XXXXXX23 | U32XXXXXX24
  | U32XXXXXX25 | U32XXXXXX26 | U32XXXXXX27 | U32XXXXXX28
  | U32XXXXXX29 | U32XXXXXX30 | U32XXXXXX31 | U32XXXXXX32
  deriving (Bounded, Enum, Generic, NFData, Show, Typeable)

instance FromField U32GenericStripPrefix where
  parseField = genericParseField defaultOptions{fieldLabelModifier = dropPrefix "U32"}

instance ToField U32GenericStripPrefix where
  toField = genericToField defaultOptions{fieldLabelModifier = dropPrefix "U32"}
