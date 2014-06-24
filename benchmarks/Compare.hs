{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------
-- | Compare the output from two different benchmark runs.
--
-- Benchmark two branches then run this tool:
--
-- > git checkout first-branch
-- > cabal bench --benchmark-option='-uold.csv'
-- > git checkout second-branch
-- > cabal bench --benchmark-option='-unew.csv'
-- > dist/build/compare/compare old.csv new.csv
module Main (main) where

------------------------------------------------------------------------
import Control.Applicative
import Control.Monad.Error (ErrorT (..), runErrorT)
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Prelude hiding (compare)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)

------------------------------------------------------------------------
data Reading = Reading Text Double

instance FromNamedRecord Reading where
    parseNamedRecord m = Reading <$> m .: "Name"
                                 <*> m .: "Mean"

------------------------------------------------------------------------
data Diff = Diff Text Double Double Double

------------------------------------------------------------------------
compare :: Reading -> Reading -> Diff            
compare (Reading label x) (Reading _ 0) = Diff label x 0 0
compare (Reading label x) (Reading _ y) = Diff label x y (y / x * 100.0)

------------------------------------------------------------------------
merge :: Vector Reading -> Vector Reading -> Map Text [Reading]
merge old new = fold old (fold new M.empty)
  where
    fold :: Vector Reading -> Map Text [Reading] -> Map Text [Reading]
    fold xs acc = V.foldr insert acc xs

    insert :: Reading -> Map Text [Reading] -> Map Text [Reading]
    insert r@(Reading label _) map = M.insertWith (++) label [r] map

------------------------------------------------------------------------
diff :: Map Text [Reading] -> [Diff]
diff = M.fold go []
  where
    go :: [Reading] -> [Diff] -> [Diff]
    go [old, new] xs = compare old new : xs
    go _          xs = xs -- Skip wrong number of elements caused by
                          -- invalid CSV files.

------------------------------------------------------------------------
pretty :: [Diff] -> IO ()
pretty xs = mapM_ (line $ len xs) xs
  where
    line :: Int -> Diff -> IO ()
    line pad (Diff label old new percent) =
      putStrLn (str pad label ++ "  " ++
                num old       ++ "  " ++
                num new       ++ "  " ++
                per percent   ++ "%")

    len :: [Diff] -> Int
    len = foldr (\(Diff a _ _ _) b -> max (T.length a) b) 0

    num :: Double -> String
    num = printf "%08.2E"

    per :: Double -> String
    per = printf "%5.1f"
    
    str :: Int -> Text -> String
    str padding = T.unpack . T.justifyLeft padding ' '
    
------------------------------------------------------------------------
decodeFile :: FilePath -> IO (Either String (Vector Reading))
decodeFile name = do
  contents <- BL.readFile name
  return (snd <$> decodeByName contents)

------------------------------------------------------------------------
decodeAndPrint :: FilePath -> FilePath -> IO ()
decodeAndPrint fileA fileB = do
  readings <- runErrorT $ do
    readingsA <- ErrorT $ decodeFile fileA
    readingsB <- ErrorT $ decodeFile fileB
    return (readingsA, readingsB)

  case readings of
    Left err         -> fail err
    Right (old, new) -> pretty (diff $ merge old new)

------------------------------------------------------------------------
main :: IO ()
main = do
  args <- getArgs

  case args of
    (fileA:fileB:[]) -> decodeAndPrint fileA fileB
    _                -> putStrLn "Usage: convert old.csv new.csv" >>
                        exitFailure
