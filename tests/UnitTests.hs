{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Traversable
import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Word
import Test.HUnit
import Test.Framework as TF
import Test.Framework.Providers.HUnit as TF
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 as TF

import Data.Ceason
import Data.Ceason.Parser
import Data.Ceason.Types

------------------------------------------------------------------------
-- Parse tests

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

parseTests :: [TF.Test]
parseTests =
    [ TF.testCase "simple" testSimple
    , TF.testCase "crlf" testCrlf
    , TF.testCase "rfc4180" testRfc4180
    , TF.testCase "noEol" testNoEol
    , TF.testCase "blankLine" testBlankLine
    , TF.testCase "leadingSpace" testLeadingSpace
    ]

------------------------------------------------------------------------
-- Conversion tests

instance Arbitrary B.ByteString where
    arbitrary = B.pack `fmap` arbitrary

instance Arbitrary BL.ByteString where
    arbitrary = BL.fromChunks `fmap` arbitrary

instance Arbitrary T.Text where
    arbitrary = T.pack `fmap` arbitrary

instance Arbitrary LT.Text where
    arbitrary = LT.fromChunks `fmap` arbitrary

roundTrip :: (Eq a, FromField a, ToField a) => a -> Bool
roundTrip x = case decode (encode (V.singleton (Only x))) of
    Right v 
        | V.length v == 1 -> let (Only y) = v ! 0 in x == y
    _        -> False

conversionTests :: [TF.Test]
conversionTests =
    [ TF.testProperty "roundTrip/Char" (roundTrip :: Char -> Bool)
    , TF.testProperty "roundTrip/ByteString" (roundTrip :: B.ByteString -> Bool)
    , TF.testProperty "roundTrip/Int" (roundTrip :: Int -> Bool)
    , TF.testProperty "roundTrip/Integer" (roundTrip :: Integer -> Bool)
    , TF.testProperty "roundTrip/Int8" (roundTrip :: Int8 -> Bool)
    , TF.testProperty "roundTrip/Int16" (roundTrip :: Int16 -> Bool)
    , TF.testProperty "roundTrip/Int32" (roundTrip :: Int32 -> Bool)
    , TF.testProperty "roundTrip/Int64" (roundTrip :: Int64 -> Bool)
    , TF.testProperty "roundTrip/Word" (roundTrip :: Word -> Bool)
    , TF.testProperty "roundTrip/Word8" (roundTrip :: Word8 -> Bool)
    , TF.testProperty "roundTrip/Word16" (roundTrip :: Word16 -> Bool)
    , TF.testProperty "roundTrip/Word32" (roundTrip :: Word32 -> Bool)
    , TF.testProperty "roundTrip/Word64" (roundTrip :: Word64 -> Bool)
    , TF.testProperty "roundTrip/lazy ByteString"
      (roundTrip :: BL.ByteString -> Bool)
    , TF.testProperty "roundTrip/Text" (roundTrip :: T.Text -> Bool)
    , TF.testProperty "roundTrip/lazy Text" (roundTrip :: LT.Text -> Bool)
    ]

------------------------------------------------------------------------
-- Test harness

allTests :: [TF.Test]
allTests = parseTests ++ conversionTests

main :: IO ()
main = defaultMain allTests
