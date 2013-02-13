module Data.Csv.Types
    (
    -- * Core CSV types
      Csv
    , Record
    , Header
    , Name
    , NamedRecord
    , Field
    , toNamedRecord
    ) where

import qualified Data.ByteString as S
import qualified Data.HashMap.Strict as HM
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | CSV data represented as a Haskell vector of vector of
-- bytestrings.
type Csv = Vector Record

-- | A record corresponds to a single line in a CSV file.
type Record = Vector Field

-- | The header corresponds to the first line a CSV file. Not all CSV
-- files have a header.
type Header = Vector Name

-- | A header has one or more names, describing the data in the column
-- following the name.
type Name = S.ByteString

-- | A record corresponds to a single line in a CSV file, indexed by
-- the column name rather than the column index.
type NamedRecord = HM.HashMap S.ByteString S.ByteString

-- | A single field within a record.
type Field = S.ByteString

-- | Convert a 'Record' to a 'NamedRecord' by attaching column names.
-- The 'Header' and 'Record' must be of the same length.
toNamedRecord :: Header -> Record -> NamedRecord
toNamedRecord hdr v = HM.fromList . V.toList $ V.zip hdr v
