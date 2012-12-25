{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings,
             TypeSynonymInstances #-}
module Main ( main ) where

import Control.Applicative
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Vector (Vector)

import Data.Csv

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

instance ToNamedRecord President where
    toNamedRecord (presidency, president, wikipediaEntry, tookOffice,
                   leftOffice, party, homeState) = namedRecord
        [ "Presidency"      .= presidency
        , "President"       .= president
        , "Wikipedia Entry" .= wikipediaEntry
        , "Took office"     .= tookOffice
        , "Left office"     .= leftOffice
        , "Party"           .= party
        , "Home State"      .= homeState
        ]


fromStrict s = BL.fromChunks [s]

type BSHashMap a = HM.HashMap B.ByteString a

main :: IO ()
main = do
    !csvData <- fromStrict `fmap` B.readFile "benchmarks/presidents.csv"
    !csvDataN <- fromStrict `fmap` B.readFile
                 "benchmarks/presidents_with_header.csv"
    let (Right !presidents) = decodePresidents csvData
        (Right (!hdr, !presidentsN)) = decodePresidentsN csvDataN
    defaultMain [
          bgroup "positional"
          [ bgroup "decode"
            [ bench "presidents/without conversion" $ whnf idDecode csvData
            , bench "presidents/with conversion" $ whnf decodePresidents csvData
            ]
          , bgroup "encode"
            [ bench "presidents/with conversion" $ whnf encode presidents
            ]
          ]
        , bgroup "named"
          [ bgroup "decode"
            [  bench "presidents/without conversion" $ whnf idDecodeN csvDataN
            , bench "presidents/with conversion" $ whnf decodePresidentsN csvDataN
            ]
          , bgroup "encode"
            [ bench "presidents/with conversion" $ whnf (encodeByName hdr) presidentsN
            ]
          ]
        ]
  where
    decodePresidents :: BL.ByteString -> Either String (Vector President)
    decodePresidents = decode False

    decodePresidentsN :: BL.ByteString -> Either String (Header, Vector President)
    decodePresidentsN = decodeByName

    idDecode :: BL.ByteString -> Either String (Vector (Vector B.ByteString))
    idDecode = decode False

    idDecodeN :: BL.ByteString -> Either String (Header, Vector (BSHashMap B.ByteString))
    idDecodeN = decodeByName
