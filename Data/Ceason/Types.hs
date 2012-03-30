module Data.Ceason.Types
    (
    -- * Core CSV types
      Csv
    , Record
    , Field
    , Parser
    , Result(..)
    , parse
    , parseEither
    , parseMaybe

    -- * Type conversion
    , Only(..)
    , FromRecord(..)
    , FromField(..)
    , ToRecord(..)
    , ToField(..)

    -- * Accessors
    , (.!)
    ) where

import Data.Ceason.Types.Class
import Data.Ceason.Types.Internal
