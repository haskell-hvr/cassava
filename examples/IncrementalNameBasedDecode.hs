{-# LANGUAGE BangPatterns, ScopedTypeVariables, OverloadedStrings #-}

import Control.Monad
import qualified Data.ByteString as B
import Data.Csv (FromNamedRecord(..), (.:))
import Data.Csv.Incremental
import System.Exit
import System.IO
import Data.Either (rights)

data SalaryInfo = SalaryInfo
    { name :: String
    , salary :: Int
    }

instance FromNamedRecord SalaryInfo where
    parseNamedRecord m = SalaryInfo <$>
                         m .: "name" <*>
                         m .: "salary"

main :: IO ()
main = withFile "salaries.csv" ReadMode $ \ csvFile -> do

    let headerLoop (FailH _ errMsg) = putStrLn errMsg >> exitFailure
        headerLoop (PartialH fn) = headerLoop =<< feedHeader fn
        headerLoop (DoneH header parser) = loop 0 parser

        loop !_ (Fail _ errMsg) = putStrLn errMsg >> exitFailure
        loop acc (Many rs k)    = loop (acc + sumSalaries rs) =<< feed k
        loop acc (Done rs)      = putStrLn $ "Total salaries: " ++
                                  show (sumSalaries rs + acc)

        feedHeader k = do
          isEof <- hIsEOF csvFile
          if isEof
                then return $ k B.empty
                else k `fmap` B.hGetSome csvFile 4096

        feed k = do
            isEof <- hIsEOF csvFile
            if isEof
                then return $ k B.empty
                else k `fmap` B.hGetSome csvFile 4096
    headerLoop (decodeByName :: HeaderParser (Parser SalaryInfo))
  where
    sumSalaries :: [Either String SalaryInfo] -> Int
    sumSalaries rs = 
      let good = rights rs
      in sum $ map (\sinfo -> salary sinfo) good
