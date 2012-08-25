{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Csv.Conversion.Generics as G
import qualified Data.Vector as V
import GHC.Generics

data Person = Person String Int deriving Generic

instance FromRecord Person where
    parseRecord = G.parseRecord
instance ToRecord Person where
    toRecord = G.toRecord

persons :: [Person]
persons = [Person "John" 50000, Person "Jane" 60000]

main :: IO ()
main = do
    BL.writeFile "salaries.csv" $ encode (V.fromList persons)
    csvData <- BL.readFile "salaries.csv"
    case decode csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (Person name salary) ->
            putStrLn $ name ++ " earns " ++ show salary ++ " dollars"
