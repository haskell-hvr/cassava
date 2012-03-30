{-# LANGUAGE BangPatterns, Rank2Types #-}

-- | RFC 4180 compliant CSV parsing and encodig.
module Data.Ceason
    (
    -- * Encoding and decoding
      decode
    , encode

    -- * Core CSV types
    , Csv
    , Record
    , Field

    -- * Type conversion
    , Only(..)
    , FromRecord(..)
    , FromField(..)
    , ToRecord(..)
    , ToField(..)

    -- * Accessors
    , (.!)
    ) where

import qualified Data.ByteString.Lazy as L
import Data.Traversable
import Data.Vector (Vector)

import Data.Ceason.Encode
import Data.Ceason.Parser.Internal
import Data.Ceason.Types

-- | Efficiently deserialize CSV records from a lazy
-- 'L.ByteString'. If this fails due to incomplete or invalid input,
-- 'Nothing' is returned.
decode :: FromRecord a => L.ByteString -> Maybe (Vector a)
decode = decodeWith csv (parse . traverse parseRecord)
