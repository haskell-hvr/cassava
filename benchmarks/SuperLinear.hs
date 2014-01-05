{-# LANGUAGE BangPatterns #-}

-- | Tests that runtime and space usage is linear in the number of rows.
-- Run with e.g.
--
-- > cabal bench super-linear --benchmark-option='+RTS' --benchmark-option='-hy'
--
-- You can provide an optional argument specifying the number of rows
-- to use in the benchmark.
module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Environment (getArgs)
import System.Directory
import System.IO

type DP = VU.Vector Float

loadData :: FilePath -> IO (V.Vector DP)
loadData filename = do
    bs <- BS.readFile filename
    rs <- case decode NoHeader bs of
        Right rs -> return rs
        Left str -> error $ "failed to parse CSV file " ++ filename ++ ": " ++
                    take 1000 str
    return rs

createData :: Int -> FilePath -> IO ()
createData n filename = do
    exist <- doesFileExist filename
    unless exist $ do
        putStrLn $ "Creating " ++ filename ++ "..."
        row <- BS8.pack `fmap` withFile oneLinePath ReadMode hGetLine
        withFile filename WriteMode (writeN n row)
  where
    writeN :: Int -> BS.ByteString -> Handle -> IO ()
    writeN 0 row _      = return ()
    writeN n row handle = BS8.hPutStrLn handle row >> writeN (n - 1) row handle

    oneLinePath = "benchmarks/one-line.csv"

main :: IO ()
main = do
    args <- getArgs
    let !n = case args of
                 []    -> 5000
                 [arg] -> read arg
        !filename = show n ++ "-rows.csv"
    createData n filename
    rs <- loadData filename
    evaluate $ rnf rs
