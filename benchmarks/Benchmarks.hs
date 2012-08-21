{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings,
             TypeSynonymInstances #-}
module Main ( main ) where

import Control.Applicative
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Vector (Vector)

import Data.Ceason

type President = (Int, Text, ByteString, ByteString, ByteString, Text, Text)

instance FromNamedRecord President where
    parseNamedRecord m = (,,,,,,) <$>
                         m .: "Presidency" <*>
                         m .: "President" <*>
                         m .: "Wikipedia Entry" <*>
                         m .: "Took office" <*>
                         m .: "Left office" <*>
                         m .: "Party" <*>
                         m .: "Home State"


fromStrict s = BL.fromChunks [s]

main :: IO ()
main = do
    !csvData <- fromStrict `fmap` B.readFile "benchmarks/presidents.csv"
    !csvDataH <- fromStrict `fmap` B.readFile
                 "benchmarks/presidents_with_header.csv"
    defaultMain [
          bgroup "indexed" [
               bench "presidents/without conversion" $ whnf idDecode csvData
             , bench "presidents/with conversion" $ whnf decodePresidents csvData
             ]
        , bgroup "named" [
               bench "presidents/without conversion" $ whnf idDecodeN csvDataH
             , bench "presidents/with conversion" $ whnf decodePresidentsN csvDataH
             ]
        ]
  where
    decodePresidents :: BL.ByteString -> Either String (Vector President)
    decodePresidents = decode

    decodePresidentsN :: BL.ByteString -> Either String (Header, Vector President)
    decodePresidentsN = decodeByName

    idDecode :: BL.ByteString -> Either String (Vector (Vector B.ByteString))
    idDecode = decode

    idDecodeN :: BL.ByteString -> Either String (Header, Vector (BSHashMap B.ByteString))
    idDecodeN = decodeByName
