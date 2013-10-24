{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics

data Person = Person
    { name   :: String
    , salary :: Int
    }
    deriving Generic

instance FromNamedRecord Person
instance ToNamedRecord Person

persons :: [Person]
persons = [Person "John" 50000, Person "Jane" 60000]

main :: IO ()
main = do
    BL.writeFile "salaries.csv" $ encodeByName (V.fromList ["name", "salary"]) persons
    csvData <- BL.readFile "salaries.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " earns " ++ show (salary p) ++ " dollars"
