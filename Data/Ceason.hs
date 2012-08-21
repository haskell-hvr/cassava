{-# LANGUAGE BangPatterns, Rank2Types #-}

-- | RFC 4180 compliant CSV parsing and encodig.
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
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

-- | Efficiently deserialize CSV records from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, @'Left' msg@ is
-- returned. Equivalent to @'decodeWith' 'defaultDecodeOptions'@.
decode :: FromRecord a => L.ByteString -> Either String (Vector a)
decode = decodeWith defaultDecodeOptions
{-# INLINE decode #-}

-- | Efficiently deserialize CSV records from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, @'Left' msg@ is
-- returned. Equivalent to @'decodeByNameWith'
-- 'defaultDecodeOptions'@.
decodeByName :: FromNamedRecord a => L.ByteString
                 -> Either String (Header, Vector a)
decodeByName = decodeByNameWith defaultDecodeOptions
{-# INLINE decodeByName #-}

------------------------------------------------------------------------
-- ** Encoding and decoding options

-- $options
--
-- The 'decodeWith' and 'decodeByNameWith' functions lets you
-- customize how the CSV data is parsed. These can be used to e.g.
-- parse tab-separated data.

-- | Default decoding options:
--
--  * 'delimiter': comma
defaultDecodeOptions :: DecodeOptions
defaultDecodeOptions = DecodeOptions
    { delimiter  = 44  -- comma
    }

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a => DecodeOptions -> L.ByteString
           -> Either String (Vector a)
decodeWith !opts = decodeWithP (csv opts) (parse . traverse parseRecord)
{-# INLINE [1] decodeWith #-}

{-# RULES
    "idDecodeWith" decodeWith = idDecodeWith
 #-}

-- | Same as 'decodeWith', but more efficient as no type
-- conversion is performed.
idDecodeWith :: DecodeOptions -> L.ByteString
             -> Either String (Vector (Vector B.ByteString))
idDecodeWith !opts = decodeWithP (csv opts) pure

-- | Like 'decodeByName', but lets you customize how the CSV data is
-- parsed.
decodeByNameWith :: FromNamedRecord a => DecodeOptions -> L.ByteString
                     -> Either String (Header, Vector a)
decodeByNameWith !opts =
    decodeWithP (csvWithHeader opts)
    (\ (hdr, vs) -> (,) <$> pure hdr <*> (parse $ traverse parseNamedRecord vs))
