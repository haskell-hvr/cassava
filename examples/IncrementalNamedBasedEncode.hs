{-# LANGUAGE DeriveGeneric #-}
import qualified Data.ByteString.Lazy as L
import Data.Csv hiding (encodeDefaultOrderedByName)
import Data.Csv.Incremental
import Data.Monoid
import GHC.Generics

data Person = Person
    { name   :: !String
    , salary :: !Int
    }
    deriving Generic

instance FromNamedRecord Person
instance ToNamedRecord Person
instance DefaultOrdered Person

persons :: [Person]
persons = [Person "John" 50000, Person "Jane" 60000]

main = L.putStrLn $ encodeDefaultOrderedByName (go persons)
  where
    go (x:xs) = encodeNamedRecord x <> go xs
