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
import qualified Data.Attoparsec.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Traversable
import Data.Vector (Vector)

import Data.Ceason.Encode
import Data.Ceason.Parser.Internal hiding (record)
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

-- | Efficiently deserialize CSV records from a lazy
-- 'L.ByteString'. If this fails due to incomplete or invalid input,
-- 'Nothing' is returned.
decode :: FromRecord a => L.ByteString -> Either String (Vector a)
decode = decodeWith csv (parse . traverse parseRecord)

{-# RULES
    "idDecode" decode = idDecode
 #-}

-- | Same as 'decode', but more efficient as no type conversion is
-- performed.
idDecode :: L.ByteString -> Either String (Vector (Vector B.ByteString))
idDecode s = case AL.parse csv s of
      AL.Done _ v     -> Right v
      AL.Fail left _ msg -> Left $ "parse error (" ++ msg ++ ") at \"" ++
                            show (BL8.unpack left) ++ "\""

decodeWithHeader :: FromNamedRecord a => L.ByteString
                 -> Either String (Header, Vector a)
decodeWithHeader =
    decodeWith csvWithHeader
    (\ (hdr, vs) -> (,) <$> pure hdr <*> (parse $ traverse parseNamedRecord vs))
