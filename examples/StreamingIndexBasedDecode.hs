{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv.Streaming
import Data.Foldable (for_)

main :: IO ()
main = do
    csvData <- BL.readFile "salaries.csv"
    -- N.B. The Foldable instance skips records that failed to
    -- convert.
    for_ (decode NoHeader csvData) $ \ (name, salary :: Int) ->
        putStrLn $ name ++ " earns " ++ show salary ++ " dollars"
