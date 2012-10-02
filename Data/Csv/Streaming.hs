{-# LANGUAGE BangPatterns, CPP, DeriveFunctor #-}

-- | This module allows for streaming decoding of CSV data. This is
-- useful if you need to parse large amount of input in constant
-- space. The API also allow you to ignore type conversion errors on a
-- per-record basis.
module Data.Csv.Streaming
    ( Records(..)
    , decode
    , decodeWith
    , decodeByName
    , decodeByNameWith
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (Foldable(..))
import Prelude hiding (foldr)

import Data.Csv.Conversion
import Data.Csv.Incremental hiding (decode, decodeByName, decodeByNameWith,
                                    decodeWith)
import qualified Data.Csv.Incremental as I
import Data.Csv.Parser
import Data.Csv.Types

-- | A stream of parsed records. If type conversion failed for the
-- records, it's represented as @'Left' errMsg@.
data Records a
    = -- | A record or an error message, followed by more records.
      Cons (Either String a) (Records a)

      -- | End of stream, potentially due to a parse error. If a parse
      -- error occured, the first field contains the error message.
      -- The second field contains any unconsumed input.
    | Nil (Maybe String) BL.ByteString
    deriving (Eq, Functor, Show)

instance Foldable Records where
    foldr = foldrRecords
#if MIN_VERSION_base(4,6,0)
    foldl' = foldlRecords'
#endif

foldrRecords :: (a -> b -> b) -> b -> Records a -> b
foldrRecords f = go
  where
    go z (Cons (Right x) rs) = f x (go z rs)
    go z _ = z
{-# INLINE foldrRecords #-}

#if MIN_VERSION_base(4,6,0)
foldlRecords' :: (a -> b -> a) -> a -> Records b -> a
foldlRecords' f = go
  where
    go z (Cons (Right x) rs) = let z' = f z x in z' `seq` go z' rs
    go z _ = z
{-# INLINE foldlRecords' #-}
#endif

-- | Efficiently deserialize CSV records in a streaming fashion.
-- Equivalent to @'decodeWith' 'defaultDecodeOptions'@.
decode :: FromRecord a
       => Bool           -- ^ Data contains header that should be
                         -- skipped
       -> BL.ByteString  -- ^ CSV data
       -> Records a
decode = decodeWith defaultDecodeOptions

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a
           => DecodeOptions  -- ^ Decoding options
           -> Bool           -- ^ Data contains header that should be
                             -- skipped
           -> BL.ByteString  -- ^ CSV data
           -> Records a
decodeWith !opts skipHeader s0 = case BL.toChunks s0 of
    []     -> go [] (I.decodeWith opts skipHeader B.empty)
    (s:ss) -> go ss (I.decodeWith opts skipHeader s)
  where
    go ss (Done xs)       = foldr Cons (Nil Nothing (BL.fromChunks ss)) xs
    go ss (Fail rest err) = Nil (Just err) (BL.fromChunks (rest:ss))
    go [] (Partial k)     = go [] (k B.empty)
    go (s:ss) (Partial k) = go ss (k s)
    go [] (Some xs k)     = foldr Cons (go [] (k B.empty)) xs
    go (s:ss) (Some xs k) = foldr Cons (go ss (k s)) xs

-- | Efficiently deserialize CSV in a streaming fashion. The data is
-- assumed to be preceeded by a header. Returns @'Left' errMsg@ if
-- parsing the header fails. Equivalent to @'decodeByNameWith'
-- 'defaultDecodeOptions'@.
decodeByName :: FromNamedRecord a
             => BL.ByteString  -- ^ CSV data
             -> Either String (Header, Records a)
decodeByName = decodeByNameWith defaultDecodeOptions

-- | Like 'decodeByName', but lets you customize how the CSV data is
-- parsed.
decodeByNameWith :: FromNamedRecord a
                 => DecodeOptions  -- ^ Decoding options
                 -> BL.ByteString  -- ^ CSV data
                 -> Either String (Header, Records a)
decodeByNameWith !opts s0 = case BL.toChunks s0 of
    []     -> go [] (I.decodeByNameWith opts B.empty)
    (s:ss) -> go ss (I.decodeByNameWith opts s)
  where
    go ss (DoneH hdr p)    = Right (hdr, go2 ss p)
    go ss (FailH rest err) = Left err  -- TODO: Add more information
    go [] (PartialH k)     = go [] (k B.empty)
    go (s:ss) (PartialH k) = go ss (k s)

    go2 ss (Done xs)       = foldr Cons (Nil Nothing (BL.fromChunks ss)) xs
    go2 ss (Fail rest err) = Nil (Just err) (BL.fromChunks (rest:ss))
    go2 [] (Partial k)     = go2 [] (k B.empty)
    go2 (s:ss) (Partial k) = go2 ss (k s)
    go2 [] (Some xs k)     = foldr Cons (go2 [] (k B.empty)) xs
    go2 (s:ss) (Some xs k) = foldr Cons (go2 ss (k s)) xs
