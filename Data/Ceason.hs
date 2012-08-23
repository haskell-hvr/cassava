-- | This module implements parsing and encoding of CSV files. The
-- parser defined here is RFC 4180 compliant, with the following
-- extensions:
--
--  * Empty lines are ignored.
--
--  * Non-escaped fields may contain any characters except:
--    double-quotes, commas, carriage returns, and newlines
--
--  * Escaped fields may contain any characters except double-quotes.
module Data.Ceason
    (
    -- * Encoding and decoding
      decode
    , decodeByName
    , encode
    , encodeByName

    -- ** Encoding and decoding options
    -- $options
    , DecodeOptions(..)
    , defaultDecodeOptions
    , decodeWith
    , decodeByNameWith
    , EncodeOptions(..)
    , defaultEncodeOptions
    , encodeWith
    , encodeByNameWith

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
    , FromRecord(..)
    , ToRecord(..)
    , Only(..)

    -- ** Name-based record conversion
    -- $namebased
    , FromNamedRecord(..)
    , ToNamedRecord(..)
    , BSMap(..)
    , BSHashMap(..)

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

import Data.Ceason.Conversion
import Data.Ceason.Encoding
import Data.Ceason.Types

-- $typeconversion
--
-- There are two ways to convert CSV records to and from and
-- user-defined data types: index-based conversion and name-based
-- conversion.

-- $indexbased
--
-- Index-based conversion lets you convert CSV records to and from
-- user-defined data types by referring to a field's position (its
-- index) in the record. The first column in a CSV file is given index
-- 0, the second index 1, and so on.

-- $namebased
--
-- Name-based conversion lets you convert CSV records to and from
-- user-defined data types by referring to a field's name. The names
-- of the fields are defined by the first line in the file, also known
-- as the header. Name-based conversion is more robust to changes in
-- the file structure e.g. to reording or addition of columns, but can
-- be a bit slower.
