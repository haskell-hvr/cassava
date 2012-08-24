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
main = do
    csvData <- BL.readFile "salaries.csv"
    case decode csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \ (name, salary :: Int) ->
            putStrLn $ name ++ " earns " ++ show salary ++ " dollars"
```
