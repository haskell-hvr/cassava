# cassava: A CSV parsing and encoding library

A CSV parsing and encoding library optimized for ease of use and high
performance.

## Usage example

Here's the two second crash course in using the library. Given a CSV
file with this content:

```
John Doe,50000
Jane Doe,60000
```

here's how you'd process it record-by-record:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "salaries.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (name, salary :: Int) ->
            putStrLn $ name ++ " earns " ++ show salary ++ " dollars"
```

If you want to parse a file that includes a header, like this one

```
name,salary
John Doe,50000
Jane Doe,60000
```

use `decodeByName`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

data Person = Person
    { name   :: !String
    , salary :: !Int
    }

instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary"

main :: IO ()
main = do
    csvData <- BL.readFile "salaries.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " earns " ++ show (salary p) ++ " dollars"
```

## Project Goals for `cassava`

There's no end to what people consider CSV data. Most programs don't
follow [RFC4180](https://tools.ietf.org/html/rfc4180) so one has to
make a judgment call which contributions to accept.  Consequently, not
everything gets accepted, because then we'd end up with a (slow)
general purpose parsing library. There are plenty of those. The goal
is to roughly accept what the Python
[`csv`](https://docs.python.org/3/library/csv.html) module accepts.

The Python `csv` module (which is implemented in C) is also considered
the base-line for performance.  Adding options (e.g. the above
mentioned parsing "flexibility") will have to be a trade off against
performance. There's been complaints about performance in the past,
therefore, if in doubt performance wins over features.

Last but not least, it's important to keep the dependency footprint
light, as each additional dependency incurs costs and risks in terms
of additional maintenance overhead and loss of flexibility. So adding
a new package dependency should only be done if that dependency is
known to be a reliable package and there's a clear benefit which
outweights the cost.

## Further reading

The primary API documentation for `cassava` is its Haddock documentation which can be found at http://hackage.haskell.org/package/cassava/docs/Data-Csv.html 

Below are listed additional recommended third-party blogposts and tutorials

 - [CSV encoding and decoding in Haskell with Cassava](https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/)
