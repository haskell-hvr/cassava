{-# LANGUAGE BangPatterns, FlexibleInstances, OverloadedStrings,
             RecordWildCards, TypeSynonymInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Main ( main ) where

import Control.Applicative
import Control.DeepSeq
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Control.Monad (mzero)
import Data.Text (Text)
import qualified Text.CSV.Lazy.ByteString as LazyCsv
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Csv
import qualified Data.Csv.Streaming as Streaming

data President = President
                 { presidency     :: !Int
                 , president      :: !Text
                 , wikipediaEntry :: !ByteString
                 , tookOffice     :: !ByteString
                 , leftOffice     :: !ByteString
                 , party          :: !Text
                 , homeState      :: !Text
                 }

instance FromRecord President where
    parseRecord v
        | V.length v == 7 = President <$>
                            v .!! 0 <*>
                            v .!! 1 <*>
                            v .!! 2 <*>
                            v .!! 3 <*>
                            v .!! 4 <*>
                            v .!! 5 <*>
                            v .!! 6
        | otherwise       = mzero

-- | Unchecked version of '(.!)'.
(.!!) :: FromField a => Record -> Int -> Parser a
v .!! idx = parseField (V.unsafeIndex v idx)
{-# INLINE (.!!) #-}
infixl 9 .!!

instance ToRecord President where
    toRecord (President {..}) =
        record [toField presidency, toField president, toField wikipediaEntry,
                toField tookOffice, toField leftOffice, toField party,
                toField homeState]

instance FromNamedRecord President where
    parseNamedRecord m = President <$>
                         m .: "Presidency" <*>
                         m .: "President" <*>
                         m .: "Wikipedia Entry" <*>
                         m .: "Took office" <*>
                         m .: "Left office" <*>
                         m .: "Party" <*>
                         m .: "Home State"

instance ToNamedRecord President where
    toNamedRecord (President {..}) = namedRecord
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

instance NFData LazyCsv.CSVField where
    rnf LazyCsv.CSVField {} = ()
    rnf LazyCsv.CSVFieldError {} = ()

instance NFData LazyCsv.CSVError where
    rnf (LazyCsv.IncorrectRow !_ !_ !_ xs) = rnf xs
    rnf (LazyCsv.BlankLine _ _ _ field)    = rnf field
    rnf (LazyCsv.FieldError field)         = rnf field
    rnf (LazyCsv.DuplicateHeader _ s)      = rnf s
    rnf LazyCsv.NoData                     = ()

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
            [ bench "presidents/without conversion" $ whnf idDecodeN csvDataN
            , bench "presidents/with conversion" $ whnf decodePresidentsN csvDataN
            ]
          , bgroup "encode"
            [ bench "presidents/with conversion" $ whnf (encodeByName hdr) presidentsN
            ]
          ]
        , bgroup "comparison"
          [ bench "cassava" $ nf idDecode csvData
          , bench "cassava/streaming" $ nf idDecodeS csvData
          , bench "lazy-csv" $ nf LazyCsv.parseCSV csvData
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

    idDecodeS :: BL.ByteString -> Streaming.Records (Vector B.ByteString)
    idDecodeS = Streaming.decode False
