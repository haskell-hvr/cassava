#!/usr/bin/env cabal
{- cabal:
  build-depends: base >= 4, random
-}

module Main where

import System.Random
import System.Environment (getArgs)
import Data.List (intercalate)

sampleWords :: [String]
sampleWords = 
    [ "apple", "banana", "cherry", "delta", "echo", "foxtrot"
    , "golf", "hotel", "india", "juliet", "kilo", "lima"
    , "mike", "november", "oscar", "papa", "quebec", "romeo"
    , "sierra", "tango", "uniform", "victor", "whiskey", "xray"
    , "yankee", "zulu", "red", "blue", "green", "yellow"
    , "orange", "purple", "black", "white", "silver", "gold"
    ]

data CellType = IntCell | DoubleCell | StringCell
    deriving (Show, Eq, Enum, Bounded)

randomInt :: StdGen -> (Int, StdGen)
randomInt g = uniformR (1, 10000) g

randomDouble :: StdGen -> (Double, StdGen)
randomDouble g = uniformR (0.01, 9999.99) g

randomString :: StdGen -> (String, StdGen)
randomString g = 
    let (idx, g') = uniformR (0, length sampleWords - 1) g
    in (sampleWords !! idx, g')

randomCellType :: StdGen -> (CellType, StdGen)
randomCellType g = 
    let (n, g') = uniformR (0, 2) g :: (Int, StdGen)
    in (toEnum n, g')

randomCell :: CellType -> StdGen -> (String, StdGen)
randomCell IntCell g = 
    let (val, g') = randomInt g
    in (show val, g')
randomCell DoubleCell g = 
    let (val, g') = randomDouble g
    in (formatDouble val, g')
randomCell StringCell g = randomString g

formatDouble :: Double -> String
formatDouble d = 
    let rounded = fromIntegral (round (d * 100) :: Integer) / 100 :: Double
    in show rounded

generateColumnTypes :: Int -> StdGen -> ([CellType], StdGen)
generateColumnTypes 0 g = ([], g)
generateColumnTypes n g = 
    let (cellType, g') = randomCellType g
        (rest, g'') = generateColumnTypes (n - 1) g'
    in (cellType : rest, g'')

columnName :: CellType -> Int -> String
columnName IntCell n = "int_col_" ++ show n
columnName DoubleCell n = "double_col_" ++ show n
columnName StringCell n = "string_col_" ++ show n

generateHeader :: [CellType] -> String
generateHeader types = intercalate "," $ zipWith columnName types [1..]

generateCells :: [CellType] -> StdGen -> ([String], StdGen)
generateCells [] g = ([], g)
generateCells (t:ts) g = 
    let (cell, g') = randomCell t g
        (rest, g'') = generateCells ts g'
    in (cell : rest, g'')

generateRow :: [CellType] -> StdGen -> (String, StdGen)
generateRow types g = 
    let (cells, g') = generateCells types g
    in (intercalate "," cells, g')

generateRows :: Int -> [CellType] -> StdGen -> ([String], StdGen)
generateRows 0 _ g = ([], g)
generateRows n types g = 
    let (row, g') = generateRow types g
        (rest, g'') = generateRows (n - 1) types g'
    in (row : rest, g'')

generateCSV :: Int -> Int -> StdGen -> String
generateCSV numRows numCols g = 
    let (columnTypes, g') = generateColumnTypes numCols g
        header = generateHeader columnTypes
        (rows, _) = generateRows numRows columnTypes g'
    in unlines (header : rows)

outputFilename :: Int -> Int -> Int -> String
outputFilename rows cols seed = "random_r" ++ show rows ++ "_c" ++ show cols ++ "_s" ++ show seed ++ ".csv"

parseArgs :: [String] -> Maybe (Int, Int, Int)
parseArgs [rowsStr, colsStr, seedStr] = 
    Just (read rowsStr, read colsStr, read seedStr)
parseArgs _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just (rows, cols, seed) -> do
            if rows <= 0 || cols <= 0
                then putStrLn "Error: Number of rows and columns must be positive integers."
                else do
                    let g = mkStdGen seed
                        csvContent = generateCSV rows cols g
                        filename = outputFilename rows cols seed
                    writeFile filename csvContent
                    putStrLn $ "Generated " ++ filename ++ " with " ++ show rows ++ " rows and " ++ show cols ++ " columns."
        
        Nothing -> do
            putStrLn "Usage: cabal run Generate.hs -- <rows> <columns> <seed>"
            putStrLn ""
            putStrLn "Arguments:"
            putStrLn "  rows     Number of data rows to generate"
            putStrLn "  columns  Number of columns to generate"
            putStrLn "  seed     Integer seed for deterministic output"
            putStrLn ""
            putStrLn "Examples:"
            putStrLn "  cabal run Generate.hs -- 100 5 42      # Creates random_r100_c5_s42.csv"
            putStrLn "  cabal run Generate.hs -- 1000 10 12345 # Creates random_r1000_c10_s12345.csv"
