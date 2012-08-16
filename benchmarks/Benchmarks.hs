module Main ( main ) where

import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Vector (Vector)

import Data.Ceason

type President = (Int, Text, ByteString, ByteString, ByteString, Text, Text)

main :: IO ()
main = do
    csvData <- (BL.fromChunks . \ x -> [x]) `fmap` B.readFile "benchmarks/presidents.csv"
    defaultMain [
          bench "presidents" $ whnf decodePresidents csvData
        ]
  where
    decodePresidents :: BL.ByteString -> Either String (Vector President)
    decodePresidents = decode
