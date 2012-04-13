{-# LANGUAGE OverloadedStrings #-}
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

testSimple :: Assertion
testSimple = readTest "a,b,c\n" [["a", "b", "c"]]

testCrlf :: Assertion
testCrlf = readTest "a,b\r\nc,d\r\n" [["a", "b"], ["c", "d"]]

testRfc4180 :: Assertion
testRfc4180 = readTest
              (BL8.pack $
               "#field1,field2,field3\n" ++
               "\"aaa\",\"bb\n" ++
               "b\",\"ccc\"\n" ++
               "\"a,a\",\"b\"\"bb\",\"ccc\"\n" ++
               "zzz,yyy,xxx\n")
              [["#field1", "field2", "field3"],
               ["aaa", "bb\nb", "ccc"],
               ["a,a", "b\"bb", "ccc"],
               ["zzz", "yyy", "xxx"]]

testNoEol :: Assertion
testNoEol = readTest "a,b,c" [["a", "b", "c"]]

testBlankLine :: Assertion
testBlankLine = readTest "a,b,c\n\nd,e,f\n\n" [["a", "b", "c"], ["d", "e", "f"]]

testLeadingSpace :: Assertion
testLeadingSpace = readTest " a,  b,   c\n" [[" a", "  b", "   c"]]

allTests :: [TF.Test]
allTests =
    [ TF.testCase "simple" testSimple
    , TF.testCase "crlf" testCrlf
    , TF.testCase "rfc4180" testRfc4180
    , TF.testCase "noEol" testNoEol
    , TF.testCase "blankLine" testBlankLine
    , TF.testCase "leadingSpace" testLeadingSpace
    ]

main :: IO ()
main = defaultMain allTests

-- Gives better error messages in failing tests:
decode :: FromRecord a => BL.ByteString -> Either String (V.Vector a)
decode s =
    case AL.parse csv s of
        AL.Done _ v     -> parseEither (traverse parseRecord) v
        AL.Fail left _ err -> Left $ err ++ ", got " ++ show (BL8.unpack left)
