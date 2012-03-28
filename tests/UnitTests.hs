module Main
    ( main
    ) where

import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Traversable
import qualified Data.Vector as V
import Test.HUnit
import Test.Framework as TF
import Test.Framework.Providers.HUnit as TF

import Data.Ceason hiding (decode)
import Data.Ceason.Parser
import Data.Ceason.Types

readTest :: BL.ByteString -> [[B.ByteString]] -> Assertion
readTest input expected = case decode input of
    Right r  -> r @=? V.fromList (map V.fromList expected)
    Left err -> assertFailure $ "parse failure: " ++ err

testReadOddInputs :: Assertion
testReadOddInputs = do
    readTest "" []
    readTest "''" [[]]

testReadEol :: Assertion
testReadEol = do
    -- readTest "a,b" [["a","b"]]
    -- readTest "a,b\n" [["a","b"]]
    -- readTest "a,b\r\n" [["a","b"]]
    readTest "a,b\r" [["a","b"]]

allTests :: [TF.Test]
allTests =
  [ TF.testCase "readOddInputs" testReadOddInputs
  , TF.testCase "readEol" testReadEol
  ]

main :: IO ()
main = defaultMain allTests

-- Gives better error messages in failing tests:
decode :: FromRecord a => BL.ByteString -> Either String (V.Vector a)
decode s =
    case AL.parse csv s of
        AL.Done _ v     -> parseEither (traverse parseRecord) v
        AL.Fail _ _ err -> Left err
