module Data.Ceason.Types
    (
    -- * Core CSV types
      Csv
    , Record
    , Header
    , Name
    , NamedRecord
    , Field
    ) where

import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as HM
import Data.Vector (Vector)

-- | CSV data represented as a Haskell vector of vector of
-- bytestrings.
type Csv = Vector Record

-- | A record corresponds to a single line in a CSV file.
type Record = Vector Field

type Header = Vector Name

type Name = S.ByteString

-- | A record corresponds to a single line in a CSV file, indexed by
-- the column name rather than the column index.
type NamedRecord = HM.HashMap S.ByteString S.ByteString

-- | A single field within a record.
type Field = S.ByteString
