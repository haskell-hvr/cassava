{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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

decodesAs :: BL.ByteString -> [[B.ByteString]] -> Assertion
decodesAs input expected = case decode input of
    Right r  -> V.fromList (map V.fromList expected) @=? r
    Left err -> assertFailure $
                "      input: " ++ show (BL8.unpack input) ++ "\n" ++
                "parse error: " ++ err

encodesAs :: [[B.ByteString]] -> BL.ByteString -> Assertion
encodesAs input expected =
    encode (V.fromList (map V.fromList input)) @?= expected

testSimple :: Assertion
testSimple = "a,b,c\n" `decodesAs` [["a", "b", "c"]]

testCrlf :: Assertion
testCrlf = "a,b\r\nc,d\r\n" `decodesAs` [["a", "b"], ["c", "d"]]

testRfc4180 :: Assertion
testRfc4180 = (BL8.pack $
               "#field1,field2,field3\n" ++
               "\"aaa\",\"bb\n" ++
               "b\",\"ccc\"\n" ++
               "\"a,a\",\"b\"\"bb\",\"ccc\"\n" ++
               "zzz,yyy,xxx\n")
              `decodesAs`
              [["#field1", "field2", "field3"],
               ["aaa", "bb\nb", "ccc"],
               ["a,a", "b\"bb", "ccc"],
               ["zzz", "yyy", "xxx"]]

testNoEol :: Assertion
testNoEol = "a,b,c" `decodesAs` [["a", "b", "c"]]

testBlankLine :: Assertion
testBlankLine =
    "a,b,c\n\nd,e,f\n\n" `decodesAs` [["a", "b", "c"], ["d", "e", "f"]]

testLeadingSpace :: Assertion
testLeadingSpace = " a,  b,   c\n" `decodesAs` [[" a", "  b", "   c"]]

parseTests :: [TF.Test]
parseTests =
    [ testGroup "encode" $ map encodeTest
      [ ("simple",       [["abc"]],          "abc\r\n")
      , ("quoted",       [["\"abc\""]],      "\"\"\"abc\"\"\"\r\n")
      , ("quote",        [["a\"b"]],         "\"a\"\"b\"\r\n")
      , ("quotedQuote",  [["\"a\"b\""]],     "\"\"\"a\"\"b\"\"\"\r\n")
      , ("leadingSpace", [[" abc"]],         "\" abc\"\r\n")
      , ("comma",        [["abc,def"]],      "\"abc,def\"\r\n")
      , ("twoFields",    [["abc","def"]],    "abc,def\r\n")
      , ("twoRecords",   [["abc"], ["def"]], "abc\r\ndef\r\n")
      , ("newline",      [["abc\ndef"]],     "\"abc\ndef\"\r\n")
      ]
    , testGroup "decode"
      [ testCase "simple" testSimple
      , testCase "crlf" testCrlf
      , testCase "rfc4180" testRfc4180
      , testCase "noEol" testNoEol
      , testCase "blankLine" testBlankLine
      , testCase "leadingSpace" testLeadingSpace
      ]
    ]
  where
    encodeTest (name, input, expected) =
        testCase name $ input `encodesAs` expected

nameBasedTests :: [TF.Test]
nameBasedTests = []

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

-- A single column with an empty string is indistinguishable from an
-- empty line (which we will ignore.) We therefore encode at least two
-- columns.
roundTrip :: (Eq a, FromField a, ToField a) => a -> Bool
roundTrip x = case decode (encode (V.singleton (x, dummy))) of
    Right v | V.length v == 1 -> let (y, _ :: Char) = v ! 0 in x == y
    _  -> False
  where dummy = 'a'

boundary :: forall a. (Bounded a, Eq a, FromField a, ToField a) => a -> Bool
boundary _dummy = roundTrip (minBound :: a) && roundTrip (maxBound :: a)

-- TODO: Right now we only encode ASCII properly. Should we support
-- UTF-8? Arbitrary byte strings?

conversionTests :: [TF.Test]
conversionTests =
    [ testProperty "roundTrip/Char" (roundTrip :: Char -> Bool)
    , testProperty "roundTrip/ByteString" (roundTrip :: B.ByteString -> Bool)
    , testProperty "roundTrip/Int" (roundTrip :: Int -> Bool)
    , testProperty "roundTrip/Integer" (roundTrip :: Integer -> Bool)
    , testProperty "roundTrip/Int8" (roundTrip :: Int8 -> Bool)
    , testProperty "roundTrip/Int16" (roundTrip :: Int16 -> Bool)
    , testProperty "roundTrip/Int32" (roundTrip :: Int32 -> Bool)
    , testProperty "roundTrip/Int64" (roundTrip :: Int64 -> Bool)
    , testProperty "roundTrip/Word" (roundTrip :: Word -> Bool)
    , testProperty "roundTrip/Word8" (roundTrip :: Word8 -> Bool)
    , testProperty "roundTrip/Word16" (roundTrip :: Word16 -> Bool)
    , testProperty "roundTrip/Word32" (roundTrip :: Word32 -> Bool)
    , testProperty "roundTrip/Word64" (roundTrip :: Word64 -> Bool)
    , testProperty "roundTrip/lazy ByteString"
      (roundTrip :: BL.ByteString -> Bool)
    , testProperty "roundTrip/Text" (roundTrip :: T.Text -> Bool)
    , testProperty "roundTrip/lazy Text" (roundTrip :: LT.Text -> Bool)
    , testProperty "boundary/Int" (boundary (undefined :: Int))
    , testProperty "boundary/Int8" (boundary (undefined :: Int8))
    , testProperty "boundary/Int16" (boundary (undefined :: Int16))
    , testProperty "boundary/Int32" (boundary (undefined :: Int32))
    , testProperty "boundary/Int64" (boundary (undefined :: Int64))
    , testProperty "boundary/Word" (boundary (undefined :: Word))
    , testProperty "boundary/Word8" (boundary (undefined :: Word8))
    , testProperty "boundary/Word16" (boundary (undefined :: Word16))
    , testProperty "boundary/Word32" (boundary (undefined :: Word32))
    , testProperty "boundary/Word64" (boundary (undefined :: Word64))
    ]

------------------------------------------------------------------------
-- Test harness

allTests :: [TF.Test]
allTests = parseTests ++ nameBasedTests ++ conversionTests

main :: IO ()
main = defaultMain allTests
