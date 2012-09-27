{-# LANGUAGE BangPatterns, DeriveFunctor #-}

module Data.Csv.Streaming
    ( Records(..)
    , decode
    , decodeWith
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Csv.Conversion
import Data.Csv.Incremental hiding (decode, decodeWith)
import qualified Data.Csv.Incremental as I
import Data.Csv.Parser

data Records a = Cons (Either String a) (Records a)
               | Nil (Maybe String) BL.ByteString
               deriving (Eq, Functor, Show)

decode :: FromRecord a
       => Bool           -- ^ Data contains header that should be
                         -- skipped
       -> BL.ByteString  -- ^ CSV data
       -> Records a
decode = decodeWith defaultDecodeOptions

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
