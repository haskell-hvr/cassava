{-# LANGUAGE BangPatterns, Rank2Types #-}

-- | RFC 4180 compliant CSV parsing and encodig.
module Data.Ceason
    (
    -- * Encoding and decoding
      decode
    , decodeWithHeader
    , encode

    -- * Core CSV types
    , Csv
    , Record
    , Field
    , Header
    , Name
    , NamedRecord

    -- * Type conversion
    -- $typeconversion

    -- ** Index-based record conversion
    -- $indexbased
    , Only(..)
    , FromRecord(..)
    , ToRecord(..)

    -- ** Name-based record conversion
    -- $namebased
    , BSMap(..)
    , BSHashMap(..)
    , FromNamedRecord(..)
    , ToNamedRecord(..)

    -- ** Field conversion
    , FromField(..)
    , ToField(..)

    -- * Accessors
    , (.!)
    , (.:)
    , (.=)
    , record
    , namedRecord
    ) where

import Control.Applicative
import qualified Data.ByteString.Lazy as L
import Data.Traversable
import Data.Vector (Vector)

import Data.Ceason.Encode
import Data.Ceason.Parser.Internal hiding (record)
import Data.Ceason.Types

-- $typeconversion
--
-- There are two ways to convert between CSV records and data types:
-- index based and name based.

-- $indexbased
--
-- Index-based conversion lets you convert CSV records to data types
-- by referring to a field's position (its index) in the file.  The
-- first column in a CSV file is given index 0, the second index 1,
-- and so on.

-- $namebased
--
-- Name-based conversion lets you convert CSV records to data types by
-- referring to a field's name.  The names of the fields are defined
-- by the first line in the file, also known as the header.
-- Name-based conversion is more robust to changes in the file
-- structure e.g. to reording or addition of columns, but can be a bit
-- slower.

-- | Efficiently deserialize CSV records from a lazy
-- 'L.ByteString'. If this fails due to incomplete or invalid input,
-- 'Nothing' is returned.
decode :: FromRecord a => L.ByteString -> Maybe (Vector a)
decode = decodeWith csv (parse . traverse parseRecord)

decodeWithHeader :: FromNamedRecord a => L.ByteString
                 -> Either String (Header, Vector a)
decodeWithHeader =
    decodeWithEither csvWithHeader
    (\ (hdr, vs) -> (,) <$> pure hdr <*> (parse $ traverse parseNamedRecord vs))
