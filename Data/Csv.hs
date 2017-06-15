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
    -- * Usage examples
    -- $example

    -- ** Encoding and decoding custom data types
    -- $example-instance

    -- *** Index-based record conversion
    -- $example-indexed-instance

    -- *** Name-based record conversion
    -- $example-named-instance

    -- * Treating CSV data as opaque byte strings
    -- $generic-processing

    -- * Custom type conversions for fields
    -- $customtypeconversions

    -- ** Dealing with bad data
    -- $baddata

    -- * Encoding and decoding
    -- $encoding
      HasHeader(..)
    , decode
    , decodeByName
    , encode
    , encodeByName
    , encodeDefaultOrderedByName
    , DefaultOrdered(..)

    -- ** Encoding and decoding options
    -- $options
    , DecodeOptions(..)
    , defaultDecodeOptions
    , decodeWith
    , decodeWith'
    , decodeByNameWith
    , decodeByNameWith'
    , EncodeOptions(..)
    , Quoting(..)
    , defaultEncodeOptions
    , encodeWith
    , encodeByNameWith
    , encodeDefaultOrderedByNameWith

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
    , unsafeIndex
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
    , header

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
-- Encoding standard Haskell types:
--
-- > >>> encode [("John" :: Text, 27), ("Jane", 28)]
-- > "John,27\r\nJane,28\r\n"
--
-- Since string literals are overloaded we have to supply a type
-- signature as the compiler couldn't deduce which string type (i.e.
-- 'String' or 'Text') we want to use. In most cases type inference
-- will infer the type from the context and you can omit type
-- signatures.
--
-- Decoding standard Haskell types:
--
-- > >>> decode NoHeader "John,27\r\nJane,28\r\n" :: Either String (Vector (Text, Int))
-- > Right [("John",27),("Jane",28)]
--
-- We pass 'NoHeader' as the first argument to indicate that the CSV
-- input data isn't preceded by a header.
--
-- In practice, the return type of 'decode' rarely needs to be given,
-- as it can often be inferred from the context.

-- $example-instance
--
-- To encode and decode your own data types you need to defined
-- instances of either 'ToRecord' and 'FromRecord' or 'ToNamedRecord'
-- and 'FromNamedRecord'. The former is used for encoding/decoding
-- using the column index and the latter using the column name.
--
-- There are two ways to to define these instances, either by manually
-- defining them or by using GHC generics to derive them automatically.

-- $example-indexed-instance
--
-- Derived:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > data Person = Person { name :: !Text , salary :: !Int }
-- >     deriving Generic
-- >
-- > instance FromRecord Person
-- > instance ToRecord Person
--
-- Manually defined:
--
-- > data Person = Person { name :: !Text , salary :: !Int }
-- >
-- > instance FromRecord Person where
-- >     parseRecord v
-- >         | length v == 2 = Person <$> v .! 0 <*> v .! 1
-- >         | otherwise     = mzero
-- > instance ToRecord Person where
-- >     toRecord (Person name age) = record [
-- >         toField name, toField age]
--
-- We can now use e.g. 'encode' and 'decode' to encode and decode our
-- data type.
--
-- Encoding:
--
-- > >>> encode [Person ("John" :: Text) 27]
-- > "John,27\r\n"
--
-- Decoding:
--
-- > >>> decode NoHeader "John,27\r\n" :: Either String (Vector Person)
-- > Right [Person {name = "John", salary = 27}]
--

-- $example-named-instance
--
-- Derived:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > data Person = Person { name :: !Text , salary :: !Int }
-- >     deriving Generic
-- >
-- > instance FromNamedRecord Person
-- > instance ToNamedRecord Person
-- > instance DefaultOrdered Person
--
-- Manually defined:
--
-- > data Person = Person { name :: !Text , salary :: !Int }
-- >
-- > instance FromNamedRecord Person where
-- >     parseNamedRecord m = Person <$> m .: "name" <*> m .: "salary"
-- > instance ToNamedRecord Person where
-- >     toNamedRecord (Person name salary) = namedRecord [
-- >         "name" .= name, "salary" .= salary]
-- > instance DefaultOrdered Person where
-- >     headerOrder _ = header ["name", "salary"]
--
-- We can now use e.g. 'encodeDefaultOrderedByName' (or 'encodeByName'
-- with an explicit header order) and 'decodeByName' to encode and
-- decode our data type.
--
-- Encoding:
--
-- > >>> encodeDefaultOrderedByName [Person ("John" :: Text) 27]
-- > "name,salary\r\nJohn,27\r\n"
--
-- Decoding:
--
-- > >>> decodeByName "name,salary\r\nJohn,27\r\n" :: Either String (Header, Vector Person)
-- > Right (["name","salary"],[Person {name = "John", salary = 27}])
--

-- $generic-processing
--
-- Sometimes you might want to work with a CSV file which contents is
-- unknown to you. For example, you might want remove the second
-- column of a file without knowing anything about its content. To
-- parse a CSV file to a generic representation, just convert each
-- record to a @'Vector' 'ByteString'@ value, like so:
--
-- > >>> decode NoHeader "John,27\r\nJane,28\r\n" :: Either String (Vector (Vector ByteString))
-- > Right [["John","27"],["Jane","28"]]
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
-- If a field might be in one several different formats, you can use a
-- newtype to normalize the result:
--
-- > newtype HexOrDecimal = HexOrDecimal Int
-- >
-- > instance FromField DefaultToZero where
-- >     parseField s = case runParser (parseField s :: Parser Hex) of
-- >         Left err -> HexOrDecimal <$> parseField s  -- Uses Int instance
-- >         Right n  -> pure $ HexOrDecimal n
--
-- You can use the unit type, @()@, to ignore a column. The
-- 'parseField' method for @()@ doesn't look at the 'Field' and thus
-- always decodes successfully. Note that it lacks a corresponding
-- 'ToField' instance. Example:
--
-- > case decode NoHeader "foo,1\r\nbar,22" of
-- >     Left  err -> putStrLn err
-- >     Right v   -> forM_ v $ \ ((), i) -> print (i :: Int)

-- $baddata
--
-- If your input might contain invalid fields, you can write a custom
-- 'FromField' instance to deal with them. Example:
--
-- > newtype DefaultToZero = DefaultToZero Int
-- >
-- > instance FromField DefaultToZero where
-- >     parseField s = case runParser (parseField s) of
-- >         Left err -> pure $ DefaultToZero 0
-- >         Right n  -> pure $ DefaultToZero n

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
