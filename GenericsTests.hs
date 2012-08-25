{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}
module Main where

import GHC.Generics(Generic)
import qualified Data.Vector as V

-- I don't want to muck around in UnitTests.hs, but this illustrates extent of
-- testing done:

import Data.Csv


-- product records:
data Only' a = Only' a deriving (Show, Generic)
instance (FromField a)=> FromRecord (Only' a) 

singleField = decode "John\nJack\n\nJane\n" :: Either String (V.Vector (Only' String))

data Two a b = Two a b deriving (Show, Generic)
instance (FromField a, FromField b)=> FromRecord (Two a b)


-- a product type for records of 1-2 fields
data SumAlt a b = L a
                | R a b
                deriving (Show, Generic)
instance (FromField a, FromField b)=> FromRecord (SumAlt a b)

twoRecordTypes = decode "John,1\n\nJack" :: Either String (V.Vector (SumAlt String Int))




-- A constructor supporting failure.
-- Unfortunately (even if constructor order was correct (see below)) we can't
-- ask for a `Maybe (String,Int)` because we have two different classes for
-- FromRecord and FromField. Could these be merged?
data SumAlt2 a b = JustFields a b
                 | NothingRec
                 deriving (Generic, Show)
instance (FromField a, FromField b)=> FromRecord (SumAlt2 a b)

someFail = decode "John,1\nJack,PLORT\nJane,3" :: Either String (V.Vector (SumAlt2 String Int))

-- Awkwardly, we can't derive an instance for Maybe automatically since the
-- order of constructors goes: 
--     Nothing | Just a  
-- wonder if this deserves a ticket of some kind?
instance FromField a => FromRecord (Maybe a)

tragicallyAllNothing = decode "John\r\nJane\r\n" :: Either String (V.Vector (Maybe String))
