{-# LANGUAGE BangPatterns, CPP, DeriveFunctor #-}

-- | This module allows for streaming decoding of CSV data. This is
-- useful if you need to parse large amounts of input in constant
-- space. The API also allows you to ignore type conversion errors on
-- a per-record basis.
module Data.Csv.Streaming
    (
    -- * Usage example
    -- $example

    -- * Stream representation
    -- $stream-representation
      Records(..)

    -- * Decoding records
    -- $typeconversion

    -- ** Index-based record conversion
    -- $indexbased
    , HasHeader(..)
    , decode
    , decodeWith

    -- ** Name-based record conversion
    -- $namebased
    , decodeByName
    , decodeByNameWith
    ) where

import Control.DeepSeq (NFData(rnf))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Foldable (Foldable(..))
import Prelude hiding (foldr)

import Data.Csv.Conversion
import Data.Csv.Incremental hiding (decode, decodeByName, decodeByNameWith,
                                    decodeWith)
import qualified Data.Csv.Incremental as I
import Data.Csv.Parser
import Data.Csv.Types

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Traversable (Traversable(..))
#endif

#if !MIN_VERSION_bytestring(0,10,0)
import qualified Data.ByteString.Lazy.Internal as BL  -- for constructors
#endif

-- $example
--
-- A short usage example:
--
-- > for_ (decode NoHeader "John,27\r\nJane,28\r\n") $ \ (name, age :: Int) ->
-- >     putStrLn $ name ++ " is " ++ show age ++ " years old"
--
-- N.B. The 'Foldable' instance, which is used above, skips records
-- that failed to convert. If you don't want this behavior, work
-- directly with the 'Cons' and 'Nil' constructors.

-- $stream-representation
--
-- A stream of records is represented as a (lazy) list that may
-- contain errors.

-- $typeconversion
--
-- Just like in the case of non-streaming decoding, there are two ways
-- to convert CSV records to and from and user-defined data types:
-- index-based conversion and name-based conversion.

-- $indexbased
--
-- See documentation on index-based conversion in "Data.Csv" for more
-- information.

-- $namebased
--
-- See documentation on name-based conversion in "Data.Csv" for more
-- information.

-- | A stream of parsed records. If type conversion failed for the
-- record, the error is returned as @'Left' errMsg@.
data Records a
    = -- | A record or an error message, followed by more records.
      Cons (Either String a) (Records a)

      -- | End of stream, potentially due to a parse error. If a parse
      -- error occured, the first field contains the error message.
      -- The second field contains any unconsumed input.
    | Nil (Maybe String) BL.ByteString
    deriving (Eq, Functor, Show)

-- | Skips records that failed to convert.
instance Foldable Records where
    foldr = foldrRecords
#if MIN_VERSION_base(4,6,0)
    foldl' = foldlRecords'
#endif

foldrRecords :: (a -> b -> b) -> b -> Records a -> b
foldrRecords f = go
  where
    go z (Cons (Right x) rs) = f x (go z rs)
    go z (Cons (Left _) rs) = go z rs
    go z _ = z
{-# INLINE foldrRecords #-}

#if MIN_VERSION_base(4,6,0)
foldlRecords' :: (a -> b -> a) -> a -> Records b -> a
foldlRecords' f = go
  where
    go z (Cons (Right x) rs) = let z' = f z x in z' `seq` go z' rs
    go z (Cons (Left _) rs) = go z rs
    go z _ = z
{-# INLINE foldlRecords' #-}
#endif

instance Traversable Records where
    traverse _ (Nil merr rest) = pure $ Nil merr rest
    traverse f (Cons x xs)     = Cons <$> traverseElem x <*> traverse f xs
      where
        traverseElem (Left err) = pure $ Left err
        traverseElem (Right y)  = Right <$> f y

instance NFData a => NFData (Records a) where
    rnf (Cons r rs) = rnf r `seq` rnf rs
#if MIN_VERSION_bytestring(0,10,0)
    rnf (Nil errMsg rest) = rnf errMsg `seq` rnf rest
#else
    rnf (Nil errMsg rest) = rnf errMsg `seq` rnfLazyByteString rest

rnfLazyByteString :: BL.ByteString -> ()
rnfLazyByteString BL.Empty       = ()
rnfLazyByteString (BL.Chunk _ b) = rnfLazyByteString b
#endif

-- | Efficiently deserialize CSV records in a streaming fashion.
-- Equivalent to @'decodeWith' 'defaultDecodeOptions'@.
decode :: FromRecord a
       => HasHeader      -- ^ Data contains header that should be
                         -- skipped
       -> BL.ByteString  -- ^ CSV data
       -> Records a
decode = decodeWith defaultDecodeOptions

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a
           => DecodeOptions  -- ^ Decoding options
           -> HasHeader      -- ^ Data contains header that should be
                             -- skipped
           -> BL.ByteString  -- ^ CSV data
           -> Records a
decodeWith !opts hasHeader s0 =
    go (BL.toChunks s0) (I.decodeWith opts hasHeader)
  where
    go ss (Done xs)       = foldr Cons (Nil Nothing (BL.fromChunks ss)) xs
    go ss (Fail rest err) = Nil (Just err) (BL.fromChunks (rest:ss))
    go [] (Many xs k)     = foldr Cons (go [] (k B.empty)) xs
    go (s:ss) (Many xs k) = foldr Cons (go ss (k s)) xs

-- | Efficiently deserialize CSV in a streaming fashion. The data is
-- assumed to be preceded by a header. Returns @'Left' errMsg@ if
-- parsing the header fails. Equivalent to @'decodeByNameWith'
-- 'defaultDecodeOptions'@.
decodeByName :: FromNamedRecord a
             => BL.ByteString  -- ^ CSV data
             -> Either String (Header, Records a)
decodeByName = decodeByNameWith defaultDecodeOptions

-- TODO: Include something more in error messages?

-- | Like 'decodeByName', but lets you customize how the CSV data is
-- parsed.
decodeByNameWith :: FromNamedRecord a
                 => DecodeOptions  -- ^ Decoding options
                 -> BL.ByteString  -- ^ CSV data
                 -> Either String (Header, Records a)
decodeByNameWith !opts s0 = go (BL.toChunks s0) (I.decodeByNameWith opts)
  where
    go ss (DoneH hdr p)    = Right (hdr, go2 ss p)
    go ss (FailH rest err) = Left $ err ++ " at " ++
                             show (BL8.unpack . BL.fromChunks $ rest : ss)
    go [] (PartialH k)     = go [] (k B.empty)
    go (s:ss) (PartialH k) = go ss (k s)

    go2 ss (Done xs)       = foldr Cons (Nil Nothing (BL.fromChunks ss)) xs
    go2 ss (Fail rest err) = Nil (Just err) (BL.fromChunks (rest:ss))
    go2 [] (Many xs k)     = foldr Cons (go2 [] (k B.empty)) xs
    go2 (s:ss) (Many xs k) = foldr Cons (go2 ss (k s)) xs
