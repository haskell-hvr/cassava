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

import Control.Applicative
import Data.Attoparsec.Char8 (double, number, parseOnly)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as L
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Traversable
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Word
import GHC.Float (double2Float)
import Prelude hiding (takeWhile)

import Data.Ceason.Encode
import Data.Ceason.Parser.Internal
import Data.Ceason.Types

-- | Efficiently deserialize CSV records from a lazy
-- 'L.ByteString'. If this fails due to incomplete or invalid input,
-- 'Nothing' is returned.
decode :: FromRecord a => L.ByteString -> Maybe (Vector a)
decode = decodeWith csv (parse . traverse parseRecord)
