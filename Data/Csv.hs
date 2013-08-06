-- | This module implements encoding and decoding of CSV data. The
-- implementation is RFC 4180 compliant, with the following
-- extensions:
--
--  * Empty lines are ignored.
--
--  * Non-escaped fields may contain any characters except
--    double-quotes, commas, carriage returns, and newlines.
--
--  * Escaped fields may contain any characters (but double-quotes
--    need to be escaped).
module Data.Csv
    (
    -- * Usage example
    -- $example

    -- * Treating CSV data as opaque byte strings
    -- $generic-processing

    -- * Custom type conversions
    -- $customtypeconversions

    -- * Encoding and decoding
    -- $encoding
      HasHeader(..)
    , decode
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
    , runParser
    , index
    , (.!)
    , ToRecord(..)
    , record
    , Only(..)

    -- ** Name-based record conversion
    -- $namebased
    , FromNamedRecord(..)
    , lookup
    , (.:)
    , ToNamedRecord(..)
    , namedRecord
    , namedField
    , (.=)

    -- ** Field conversion
    -- $fieldconversion
    , FromField(..)
    , ToField(..)
    ) where

import Prelude hiding (lookup)

import Data.Csv.Conversion
import Data.Csv.Encoding
import Data.Csv.Types

-- $example
--
-- A short encoding usage example:
--
-- > >>> encode $ fromList [("John" :: Text, 27), ("Jane", 28)]
-- > Chunk "John,27\r\nJane,28\r\n" Empty
--
-- Since string literals are overloaded we have to supply a type
-- signature as the compiler couldn't deduce which string type (i.e.
-- 'String' or 'Text') we want to use. In most cases type inference
-- will infer the type from the context and you can omit type
-- signatures.
--
-- A short decoding usage example:
--
-- > >>> decode NoHeader "John,27\r\nJane,28\r\n" :: Either String (Vector (Text, Int))
-- > Right (fromList [("John",27),("Jane",28)])
--
-- We pass 'NoHeader' as the first argument to indicate that the CSV
-- input data isn't preceded by a header.
--
-- In practice, the return type of 'decode' rarely needs to be given,
-- as it can often be inferred from the context.

-- $generic-processing
--
-- Sometimes you might want to work with a CSV file which contents is
-- unknown to you. For example, you might want remove the second
-- column of a file without knowing anything about its content. To
-- parse a CSV file to a generic representation, just convert each
-- record to a @'Vector' 'ByteString'@ value, like so:
--
-- > decode NoHeader "John,27\r\nJane,28\r\n" :: Either String (Vector (Vector ByteString))
-- > Right (fromList [fromList ["John","27"],fromList ["Jane","28"]])
--
-- As the example output above shows, all the fields are returned as
-- uninterpreted 'ByteString' values.

-- $customtypeconversions
--
-- Most of the time the existing 'FromField' and 'ToField' instances
-- do what you want. However, if you need to parse a different format
-- (e.g. hex) but use a type (e.g. 'Int') for which there's already a
-- 'FromField' instance, you need to use a @newtype@. Example:
--
-- > newtype Hex = Hex Int
-- >
-- > parseHex :: ByteString -> Parser Int
-- > parseHex = ...
-- >
-- > instance FromField Hex where
-- >     parseField s = Hex <$> parseHex s
--
-- Other than giving an explicit type signature, you can pattern match
-- on the @newtype@ constructor to indicate which type conversion you
-- want to have the library use:
--
-- > case decode NoHeader "0xff,0xaa\r\n0x11,0x22\r\n" of
-- >     Left err -> putStrLn err
-- >     Right v  -> forM_ v $ \ (Hex val1, Hex val2) ->
-- >         print (val1, val2)
--
-- You can use the unit type, @()@, to ignore a column. The
-- 'parseField' method for @()@ doesn't look at the 'Field' and thus
-- always decodes successfully. Note that it lacks a corresponding
-- 'ToField' instance. Example:
--
-- > case decode NoHeader "foo,1\r\nbar,22" of
-- >     Left  err -> putStrLn err
-- >     Right v   -> forM_ v $ \ ((), i) -> print (i :: Int)

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

-- $fieldconversion
--
-- The 'FromField' and 'ToField' classes define how to convert between
-- 'Field's and values you care about (e.g. 'Int's). Most of the time
-- you don't need to write your own instances as the standard ones
-- cover most use cases.
