module Main
    ( main
    ) where

import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
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
    Right r  -> V.fromList (map V.fromList expected) @=? r
    Left err -> assertFailure $
                "      input: " ++ show (BL8.unpack input) ++ "\n" ++
                "parse error: " ++ err

testRfc4180 :: Assertion
testRfc4180 = readTest
              (BL8.pack $
               "#field1,field2,field3\n" ++
               "\"aaa\",\"bb\n" ++  -- XXX: Fails on embedded newline
               "b\",\"ccc\"\n" ++
               "\"a,a\",\"b\"\"bb\",\"ccc\"\n" ++
               "zzz,yyy,xxx\n")
              [["#field1", "field2", "field3"],
               ["aaa", "bb\nb", "ccc"],
               ["a,a", "b\"bb", "ccc"],
               ["zzz", "yyy", "xxx"]]

testReadOddInputs :: Assertion
testReadOddInputs = do
    readTest "" []

testReadEol :: Assertion
testReadEol = do
    readTest "a,b" [["a","b"]]
    readTest "a,b\n" [["a","b"]]
    readTest "a,b\r\n" [["a","b"]]

allTests :: [TF.Test]
allTests =
  [ TF.testCase "readOddInputs" testReadOddInputs
  , TF.testCase "readEol" testReadEol
  , TF.testCase "rfc4180" testRfc4180
  ]

main :: IO ()
main = defaultMain allTests

-- Gives better error messages in failing tests:
decode :: FromRecord a => BL.ByteString -> Either String (V.Vector a)
decode s =
    case AL.parse csv s of
        AL.Done _ v     -> parseEither (traverse parseRecord) v
        AL.Fail _ _ err -> Left err
