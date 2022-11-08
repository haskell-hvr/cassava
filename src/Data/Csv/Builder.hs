{-# LANGUAGE ScopedTypeVariables #-}

-- | Low-level bytestring builders. Most users want to use the more
-- type-safe "Data.Csv.Incremental" module instead.
module Data.Csv.Builder
    (
    -- * Encoding single records and headers
      encodeHeader
    , encodeRecord
    , encodeNamedRecord
    , encodeDefaultOrderedNamedRecord
    -- ** Encoding options
    , encodeHeaderWith
    , encodeRecordWith
    , encodeNamedRecordWith
    , encodeDefaultOrderedNamedRecordWith
    ) where

import qualified Data.Monoid as Mon

import Data.ByteString.Builder as Builder
import Data.Csv.Conversion
import qualified Data.Csv.Encoding as Encoding
import Data.Csv.Encoding (EncodeOptions(..))
import Data.Csv.Types hiding (toNamedRecord)

-- | Encode a header.
encodeHeader :: Header -> Builder.Builder
encodeHeader = encodeRecord

-- | Encode a single record.
encodeRecord :: ToRecord a => a -> Builder.Builder
encodeRecord = encodeRecordWith Encoding.defaultEncodeOptions

-- | Encode a single named record, given the field order.
encodeNamedRecord :: ToNamedRecord a =>
                     Header -> a -> Builder.Builder
encodeNamedRecord = encodeNamedRecordWith Encoding.defaultEncodeOptions

-- | Encode a single named record, using the default field order.
encodeDefaultOrderedNamedRecord ::
    (DefaultOrdered a, ToNamedRecord a) => a -> Builder.Builder
encodeDefaultOrderedNamedRecord =
    encodeDefaultOrderedNamedRecordWith Encoding.defaultEncodeOptions

-- | Like 'encodeHeader', but lets you customize how the CSV data is
-- encoded.
encodeHeaderWith :: EncodeOptions -> Header -> Builder.Builder
encodeHeaderWith = encodeRecordWith

-- | Like 'encodeRecord', but lets you customize how the CSV data is
-- encoded.
encodeRecordWith :: ToRecord a => EncodeOptions -> a -> Builder.Builder
encodeRecordWith opts r =
    Encoding.encodeRecord (encQuoting opts) (encDelimiter opts) (toRecord r)
    Mon.<> Encoding.recordSep (encUseCrLf opts)

-- | Like 'encodeNamedRecord', but lets you customize how the CSV data
-- is encoded.
encodeNamedRecordWith :: ToNamedRecord a =>
                         EncodeOptions -> Header -> a -> Builder.Builder
encodeNamedRecordWith opts hdr nr =
    Encoding.encodeNamedRecord hdr (encQuoting opts) (encDelimiter opts) (encMissing opts)
    (toNamedRecord nr) Mon.<> Encoding.recordSep (encUseCrLf opts)

-- | Like 'encodeDefaultOrderedNamedRecord', but lets you customize
-- how the CSV data is encoded.
encodeDefaultOrderedNamedRecordWith ::
    forall a. (DefaultOrdered a, ToNamedRecord a) =>
    EncodeOptions -> a -> Builder.Builder
encodeDefaultOrderedNamedRecordWith opts =
    encodeNamedRecordWith opts (headerOrder (undefined :: a))
