{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Control.Monad
import qualified Data.ByteString as B
import Data.Csv.Incremental
import System.Exit
import System.IO

main :: IO ()
main = withFile "salaries.csv" ReadMode $ \ csvFile -> do
    let loop !_ (Fail _ errMsg) = putStrLn errMsg >> exitFailure
        loop acc (Partial k)    = loop acc =<< feed k
        loop acc (Some rs k)    = loop (acc + sumSalaries rs) =<< feed k
        loop acc (Done rs)      = putStrLn $ "Total salaries: " ++
                                  show (sumSalaries rs + acc)

        feed k = do
            isEof <- hIsEOF csvFile
            if isEof
                then return $ k B.empty
                else do
                    bytes <- B.hGetSome csvFile 4096
                    return $ k bytes
    isEof <- hIsEOF csvFile
    unless isEof $ do
        bytes <- B.hGetSome csvFile 4096
        loop 0 (decode bytes)
  where
    sumSalaries rs = sum [salary | Right (_ :: String, salary :: Int) <- rs]
