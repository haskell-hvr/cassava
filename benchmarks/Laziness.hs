-- This program should run in constant space.
module Main
    ( main
    ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv

main = print $ BL.length $ encode $ replicate 10000000 $ Only (1 :: Int)
