-- | This module implements encoding and decoding of CSV data. The
-- implementation is RFC 4180 compliant, with the following
-- extensions:
--
--  * Empty lines are ignored.
--
--  * Non-escaped fields may contain any characters except
--    double-quotes, commas, carriage returns, and newlines
--
--  * Escaped fields may contain any characters (but double-quotes
--    need to be escaped).
module Data.Csv
    (
    -- * Usage example
    -- $example

    -- * Encoding and decoding
    -- $encoding
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
    , Parser
    , (.!)
    , ToRecord(..)
    , record
    , Only(..)

    -- ** Name-based record conversion
    -- $namebased
    , FromNamedRecord(..)
    , (.:)
    , ToNamedRecord(..)
    , namedRecord
    , (.=)

    -- ** Field conversion
    , FromField(..)
    , ToField(..)
    ) where

import Data.Csv.Conversion
import Data.Csv.Encoding
import Data.Csv.Types

-- $example
--
-- A short encoding usage example:
--
-- @ >>> 'encode' $ fromList [(\"John\" :: Text, 27), (\"Jane\", 28)]
-- Chunk \"John,27\\r\\nJane,28\\r\\n\" Empty
-- @
--
-- Since string literals are overloaded we have to supply a type
-- signature as the compiler couldn't deduce which string type (i.e.
-- 'String' or 'Text') we want to use. In most cases type inference
-- will infer the type from the context and you can omit type
-- signatures.
--
-- A short decoding usage example:
--
-- @ >>> 'decode' 'False' \"John,27\\r\\nJane,28\\r\\n\" :: Either String (Vector (Text, Int))
-- Right (fromList [(\"John\",27),(\"Jane\",28)])
-- @

-- $encoding
--
-- Encoding and decoding is a two step process. To encode a value, it
-- is first converted to a generic representation, using either
-- 'ToRecord' or 'ToNamedRecord'. The generic representation is then
-- encoded as CSV data. To decode a value the process is reversed and
-- either 'FromRecord' or 'FromNamedRecord' is used instead. Both
-- these steps are combined in the 'encode' and 'decode' functions.

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

-- $options
--
-- These functions can be used to control how data is encoded and
-- decoded. For example, they can be used to encode data in a
-- tab-separated format instead of in a comma-separated format.