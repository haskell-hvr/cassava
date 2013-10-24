{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics

data Person = Person String Int deriving Generic

instance FromRecord Person
instance ToRecord Person

persons :: [Person]
persons = [Person "John" 50000, Person "Jane" 60000]

main :: IO ()
main = do
    BL.writeFile "salaries.csv" $ encode persons
    csvData <- BL.readFile "salaries.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (Person name salary) ->
            putStrLn $ name ++ " earns " ++ show salary ++ " dollars"
