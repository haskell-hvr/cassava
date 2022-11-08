{-# LANGUAGE CPP, DataKinds, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 801
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}
#else
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-binds #-}
#endif

module Main
    ( main
    ) where

import Control.Applicative (Const)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.HashMap.Strict as HM
import Data.Int
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import qualified Data.Foldable as F
import Data.Word
import Numeric.Natural
import GHC.Generics (Generic)
import Test.HUnit
import Test.Framework as TF
import Test.Framework.Providers.HUnit as TF
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Framework.Providers.QuickCheck2 as TF

import Data.Csv hiding (record)
import qualified Data.Csv.Streaming as S

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif

------------------------------------------------------------------------
-- Parse tests

decodesAs :: BL.ByteString -> [[B.ByteString]] -> Assertion
decodesAs input expected = assertResult input expected $ decode NoHeader input

decodesWithAs :: DecodeOptions -> BL.ByteString -> [[B.ByteString]] -> Assertion
decodesWithAs opts input expected =
    assertResult input expected $ decodeWith opts NoHeader input

assertResult :: BL.ByteString -> [[B.ByteString]]
             -> Either String (V.Vector (V.Vector B.ByteString)) -> Assertion
assertResult input expected res = case res of
    Right r  -> V.fromList (map V.fromList expected) @=? r
    Left err -> assertFailure $
                "      input: " ++ show (BL8.unpack input) ++ "\n" ++
                "parse error: " ++ err

encodesAs :: [[B.ByteString]] -> BL.ByteString -> Assertion
encodesAs input expected =
    encode (map V.fromList input) @?= expected

encodesWithAs :: EncodeOptions -> [[B.ByteString]] -> BL.ByteString -> Assertion
encodesWithAs opts input expected =
    encodeWith opts (map V.fromList input) @?= expected

namedEncodesAs :: [B.ByteString] -> [[(B.ByteString, B.ByteString)]]
               -> BL.ByteString -> Assertion
namedEncodesAs hdr input expected =
    encodeByName (V.fromList hdr) (map HM.fromList input) @?= expected

namedEncodesWithAs :: EncodeOptions -> [B.ByteString]
                   -> [[(B.ByteString, B.ByteString)]]
                   -> BL.ByteString -> Assertion
namedEncodesWithAs opts hdr input expected =
    encodeByNameWith opts (V.fromList hdr) (map HM.fromList input) @?= expected

namedDecodesAs :: BL.ByteString -> [B.ByteString]
               -> [[(B.ByteString, B.ByteString)]] -> Assertion
namedDecodesAs input ehdr expected = case decodeByName input of
    Right r  -> (V.fromList ehdr, expected') @=? r
    Left err -> assertFailure $
                "      input: " ++ show (BL8.unpack input) ++ "\n" ++
                "parse error: " ++ err
  where
    expected' = V.fromList $ map HM.fromList expected

recordsToList :: S.Records a -> Either String [a]
recordsToList (S.Nil (Just err) _)  = Left err
recordsToList (S.Nil Nothing _)     = Right []
recordsToList (S.Cons (Left err) _) = Left err
recordsToList (S.Cons (Right x) rs) = case recordsToList rs of
    l@(Left _) -> l
    (Right xs) -> Right (x : xs)

decodesStreamingAs :: BL.ByteString -> [[B.ByteString]] -> Assertion
decodesStreamingAs input expected =
    assertResult input expected $ fmap (V.fromList . map V.fromList) $
    recordsToList $ S.decode NoHeader input

decodesWithStreamingAs :: DecodeOptions -> BL.ByteString -> [[B.ByteString]]
                       -> Assertion
decodesWithStreamingAs opts input expected =
    assertResult input expected $ fmap (V.fromList . map V.fromList) $
    recordsToList $ S.decodeWith opts NoHeader input

namedDecodesStreamingAs :: BL.ByteString -> [B.ByteString]
                        -> [[(B.ByteString, B.ByteString)]] -> Assertion
namedDecodesStreamingAs input ehdr expected = case S.decodeByName input of
    Right (hdr, rs) -> case recordsToList rs of
        Right xs -> (V.fromList ehdr, expected') @=? (hdr, xs)
        Left err -> assertFailure $
                    "           input: " ++ show (BL8.unpack input) ++ "\n" ++
                    "conversion error: " ++ err
    Left err -> assertFailure $
                "      input: " ++ show (BL8.unpack input) ++ "\n" ++
                "parse error: " ++ err
  where
    expected' = map HM.fromList expected

positionalTests :: [TF.Test]
positionalTests =
    [ testGroup "encode" $ map encodeTest
      [ ("simple",       [["abc"]],          "abc\r\n")
      , ("quoted",       [["\"abc\""]],      "\"\"\"abc\"\"\"\r\n")
      , ("quote",        [["a\"b"]],         "\"a\"\"b\"\r\n")
      , ("quotedQuote",  [["\"a\"b\""]],     "\"\"\"a\"\"b\"\"\"\r\n")
      , ("leadingSpace", [[" abc"]],         " abc\r\n")
      , ("comma",        [["abc,def"]],      "\"abc,def\"\r\n")
      , ("twoFields",    [["abc","def"]],    "abc,def\r\n")
      , ("twoRecords",   [["abc"], ["def"]], "abc\r\ndef\r\n")
      , ("newline",      [["abc\ndef"]],     "\"abc\ndef\"\r\n")
      ]
    , testGroup "encode" $ map encodeTestUnqtd
      [ ("simple",       [["abc"]],          "abc\r\n")
      , ("quoted",       [["\"abc\""]],      "\"abc\"\r\n")
      , ("quote",        [["a\"b"]],         "a\"b\r\n")
      , ("quotedQuote",  [["\"a\"b\""]],     "\"a\"b\"\r\n")
      , ("leadingSpace", [[" abc"]],         " abc\r\n")
      , ("comma",        [["abc,def"]],      "abc,def\r\n")
      , ("twoFields",    [["abc","def"]],    "abc,def\r\n")
      , ("twoRecords",   [["abc"], ["def"]], "abc\r\ndef\r\n")
      , ("newline",      [["abc\ndef"]],     "abc\ndef\r\n")
      ]
    , testGroup "encode" $ map encodeTestAllqtd
      [ ("simple",       [["abc"]],          "\"abc\"\r\n")
      , ("quoted",       [["\"abc\""]],      "\"\"\"abc\"\"\"\r\n")
      , ("quote",        [["a\"b"]],         "\"a\"\"b\"\r\n")
      , ("quotedQuote",  [["\"a\"b\""]],     "\"\"\"a\"\"b\"\"\"\r\n")
      , ("leadingSpace", [[" abc"]],         "\" abc\"\r\n")
      , ("comma",        [["abc,def"]],      "\"abc,def\"\r\n")
      , ("twoFields",    [["abc","def"]],    "\"abc\",\"def\"\r\n")
      , ("twoRecords",   [["abc"], ["def"]], "\"abc\"\r\n\"def\"\r\n")
      , ("newline",      [["abc\ndef"]],     "\"abc\ndef\"\r\n")
      ]

    , testGroup "encodeWith"
      [ testCase "tab-delim" $ encodesWithAs (defEnc { encDelimiter = 9 })
        [["1", "2"]] "1\t2\r\n"
      , testCase "newline" $ encodesWithAs (defEnc {encUseCrLf = False})
        [["1", "2"], ["3", "4"]] "1,2\n3,4\n"
      ]
    , testGroup "decode" $ map decodeTest decodeTests
    , testGroup "decodeWith" $ map decodeWithTest decodeWithTests
    , testGroup "streaming"
      [ testGroup "decode" $ map streamingDecodeTest decodeTests
      , testGroup "decodeWith" $ map streamingDecodeWithTest decodeWithTests
      ]
    ]
  where
    rfc4180Input = BL8.pack $
                   "#field1,field2,field3\n" ++
                   "\"aaa\",\"bb\n" ++
                   "b\",\"ccc\"\n" ++
                   "\"a,a\",\"b\"\"bb\",\"ccc\"\n" ++
                   "zzz,yyy,xxx\n"
    rfc4180Output = [["#field1", "field2", "field3"],
                     ["aaa", "bb\nb", "ccc"],
                     ["a,a", "b\"bb", "ccc"],
                     ["zzz", "yyy", "xxx"]]
    decodeTests =
        [ ("simple",       "a,b,c\n",        [["a", "b", "c"]])
        , ("crlf",         "a,b\r\nc,d\r\n", [["a", "b"], ["c", "d"]])
        , ("noEol",        "a,b,c",          [["a", "b", "c"]])
        , ("blankLine",    "a,b,c\n\nd,e,f\n\n",
           [["a", "b", "c"], ["d", "e", "f"]])
        , ("leadingSpace", " a,  b,   c\n",  [[" a", "  b", "   c"]])
        , ("rfc4180", rfc4180Input, rfc4180Output)
        ]
    decodeWithTests =
        [ ("tab-delim", defDec { decDelimiter = 9 }, "1\t2", [["1", "2"]])
        ]

    encodeTest (name, input, expected) =
        testCase name $ input `encodesAs` expected
    encodeTestUnqtd (name, input, expected) =
        testCase name $ encodesWithAs defEncNoneEnq input expected
    encodeTestAllqtd (name, input, expected) =
        testCase name $ encodesWithAs defEncAllEnq input expected
    decodeTest (name, input, expected) =
        testCase name $ input `decodesAs` expected
    decodeWithTest (name, opts, input, expected) =
        testCase name $ decodesWithAs opts input expected
    streamingDecodeTest (name, input, expected) =
        testCase name $ input `decodesStreamingAs` expected
    streamingDecodeWithTest (name, opts, input, expected) =
        testCase name $ decodesWithStreamingAs opts input expected
    defEnc = defaultEncodeOptions
    defEncNoneEnq = defaultEncodeOptions { encQuoting = QuoteNone }
    defEncAllEnq  = defaultEncodeOptions { encQuoting = QuoteAll  }
    defDec = defaultDecodeOptions

nameBasedTests :: [TF.Test]
nameBasedTests =
    [ testGroup "encode" $ map encodeTest
      [ ("simple", ["field"], [[("field", "abc")]], "field\r\nabc\r\n")
      , ("twoFields", ["field1", "field2"],
         [[("field1", "abc"), ("field2", "def")]],
         "field1,field2\r\nabc,def\r\n")
      , ("twoRecords", ["field"], [[("field", "abc")], [("field", "def")]],
         "field\r\nabc\r\ndef\r\n")
      ]
    , testGroup "encodeWith" $ map encodeWithTest
      [ ("no header", defEnc {encIncludeHeader = False}, ["field"],
         [[("field", "abc")]], "abc\r\n")
      ]
    , testGroup "decode" $ map decodeTest decodeTests
    , testGroup "streaming"
      [ testGroup "decode" $ map streamingDecodeTest decodeTests
      ]
    ]
  where
    decodeTests =
        [ ("simple", "field\r\nabc\r\n", ["field"], [[("field", "abc")]])
        , ("twoFields", "field1,field2\r\nabc,def\r\n", ["field1", "field2"],
           [[("field1", "abc"), ("field2", "def")]])
        , ("twoRecords", "field\r\nabc\r\ndef\r\n", ["field"],
           [[("field", "abc")], [("field", "def")]])
        , ("cr header", "field\rabc", ["field"], [[("field", "abc")]])
        , ("cr trailing", "field\rabc\r", ["field"], [[("field", "abc")]])
        , ("cr separator", "field\rabc\rdef", ["field"], [[("field", "abc")],[("field","def")]])
        ]

    encodeTest (name, hdr, input, expected) =
        testCase name $ namedEncodesAs hdr input expected
    encodeWithTest (name, opts, hdr, input, expected) =
        testCase name $ namedEncodesWithAs opts hdr input expected
    decodeTest (name, input, hdr, expected) =
        testCase name $ namedDecodesAs input hdr expected
    streamingDecodeTest (name, input, hdr, expected) =
        testCase name $ namedDecodesStreamingAs input hdr expected
    defEnc = defaultEncodeOptions

------------------------------------------------------------------------
-- Conversion tests

-- A single column with an empty string is indistinguishable from an
-- empty line (which we will ignore.) We therefore encode at least two
-- columns.
roundTrip :: (Eq a, FromField a, ToField a) => a -> Bool
roundTrip x = Right (V.fromList record) == decode NoHeader (encode record)
  where record = [(x, dummy)]
        dummy = 'a'

roundTripUnicode :: T.Text -> Assertion
roundTripUnicode x = Right (V.fromList record) @=?
                     decode NoHeader (encode record)
  where record = [(x, dummy)]
        dummy = 'a'

boundary :: forall a. (Bounded a, Eq a, FromField a, ToField a) => a -> Bool
boundary _dummy = roundTrip (minBound :: a) && roundTrip (maxBound :: a)

partialDecode :: Parser a -> Assertion
partialDecode p = case runParser p of
  Left _  -> return ()
  Right _ -> assertFailure "expected partial field decode"

expect :: (Eq a, Show a) => Parser a -> a -> Assertion
expect p a0 =
  case runParser p of
    Right a -> a @=? a0
    Left  e -> assertFailure e

conversionTests :: [TF.Test]
conversionTests =
    [ testGroup "roundTrip"
      [ testProperty "Char" (roundTrip :: Char -> Bool)
      , testProperty "ByteString" (roundTrip :: B.ByteString -> Bool)
      , testProperty "Int" (roundTrip :: Int -> Bool)
      , testProperty "Integer" (roundTrip :: Integer -> Bool)
      , testProperty "Int8" (roundTrip :: Int8 -> Bool)
      , testProperty "Int16" (roundTrip :: Int16 -> Bool)
      , testProperty "Int32" (roundTrip :: Int32 -> Bool)
      , testProperty "Int64" (roundTrip :: Int64 -> Bool)
      , testProperty "Natural" (roundTrip :: Natural -> Bool)
      , testProperty "Word" (roundTrip :: Word -> Bool)
      , testProperty "Word8" (roundTrip :: Word8 -> Bool)
      , testProperty "Word16" (roundTrip :: Word16 -> Bool)
      , testProperty "Word32" (roundTrip :: Word32 -> Bool)
      , testProperty "Word64" (roundTrip :: Word64 -> Bool)
      , testProperty "Scientific" (roundTrip :: Scientific -> Bool)
      , testProperty "lazy ByteString"
        (roundTrip :: BL.ByteString -> Bool)
      , testProperty "Text" (roundTrip :: T.Text -> Bool)
      , testProperty "lazy Text" (roundTrip :: LT.Text -> Bool)

#if __GLASGOW_HASKELL__ >= 800
      -- Using DataKinds here to prove that our Const instance is polykinded.
      , testProperty "Const Char" (roundTrip :: Const Char "" -> Bool)
#else
      -- For lower GHC versions, Const does not support PolyKinds.
      , testProperty "Const Char" (roundTrip :: Const Char () -> Bool)
#endif

      ]
    , testGroup "boundary"
      [ testProperty "Int" (boundary (undefined :: Int))
      , testProperty "Int8" (boundary (undefined :: Int8))
      , testProperty "Int16" (boundary (undefined :: Int16))
      , testProperty "Int32" (boundary (undefined :: Int32))
      , testProperty "Int64" (boundary (undefined :: Int64))
      , testProperty "Word" (boundary (undefined :: Word))
      , testProperty "Word8" (boundary (undefined :: Word8))
      , testProperty "Word16" (boundary (undefined :: Word16))
      , testProperty "Word32" (boundary (undefined :: Word32))
      , testProperty "Word64" (boundary (undefined :: Word64))
      ]
    , testGroup "Unicode"
      [ testCase "Chinese" (roundTripUnicode "我能吞下玻璃而不伤身体。")
      , testCase "Icelandic" (roundTripUnicode
                              "Sævör grét áðan því úlpan var ónýt.")
      , testCase "Turkish" (roundTripUnicode
                            "Cam yiyebilirim, bana zararı dokunmaz.")
      ]
    , testGroup "Partial Decodes"
      [ testCase "Int"         (partialDecode
                                (parseField "12.7" :: Parser Int))
      , testCase "Natural"     (partialDecode
                                (parseField "12.7" :: Parser Natural))
      , testCase "Word"        (partialDecode
                                (parseField "12.7" :: Parser Word))
      , testCase "Scientific"  (partialDecode
                                (parseField "1.0+" :: Parser Scientific))
      , testCase "Double"      (partialDecode
                                (parseField "1.0+" :: Parser Double))
      , testCase "Integer"     (partialDecode
                                (parseField "1e6"  :: Parser Integer))
      ]
    , testGroup "Space trimming"
      [ testCase "_Int"         (expect (parseField " 12"     :: Parser Int)        12)
      , testCase "Int_"         (expect (parseField "12 "     :: Parser Int)        12)
      , testCase "_Int_"        (expect (parseField " 12 "    :: Parser Int)        12)
      , testCase "_Natural"     (expect (parseField " 12"     :: Parser Natural)    12)
      , testCase "Natural_"     (expect (parseField "12 "     :: Parser Natural)    12)
      , testCase "_Natural_"    (expect (parseField " 12 "    :: Parser Natural)    12)
      , testCase "_Word"        (expect (parseField " 12"     :: Parser Word)       12)
      , testCase "Word_"        (expect (parseField "12 "     :: Parser Word)       12)
      , testCase "_Word_"       (expect (parseField " 12 "    :: Parser Word)       12)
      , testCase "_Scientific"  (expect (parseField " 1.2e1"  :: Parser Scientific) 12)
      , testCase "Scientific_"  (expect (parseField "1.2e1 "  :: Parser Scientific) 12)
      , testCase "_Scientific_" (expect (parseField " 1.2e1 " :: Parser Scientific) 12)
      , testCase "_Double"      (expect (parseField " 1.2e1"  :: Parser Double)     12)
      , testCase "Double_"      (expect (parseField "1.2e1 "  :: Parser Double)     12)
      , testCase "_Double_"     (expect (parseField " 1.2e1 " :: Parser Double)     12)
      ]
    ]

------------------------------------------------------------------------
-- Custom options tests

customDelim :: Word8 -> B.ByteString -> B.ByteString -> Property
customDelim delim f1 f2 = delim `notElem` [cr, nl, dquote] ==>
    (decodeWith decOpts NoHeader (encodeWith encOpts [V.fromList [f1, f2]]) ==
     Right (V.fromList [V.fromList [f1, f2]]))
  where
    encOpts = defaultEncodeOptions { encDelimiter = delim }
    decOpts = defaultDecodeOptions { decDelimiter = delim }
    nl = 10
    cr = 13
    dquote = 34

customMissing :: Assertion
customMissing = encodeByNameWith encOpts hdr nrs @?= ex
  where
    encOpts = defaultEncodeOptions { encMissing = id }
    hdr = V.fromList ["abc", "def"]
    nrs :: [NamedRecord]
    nrs =
      [ HM.fromList [("abc", "123")]
      , HM.fromList [("def", "456")]
      , HM.fromList [("abc", "234"), ("def", "567")]
      , HM.fromList []
      ]
    ex = "abc,def\r\n123,def\r\nabc,456\r\n234,567\r\nabc,def\r\n"

customOptionsTests :: [TF.Test]
customOptionsTests =
    [ testProperty "customDelim" customDelim
    , testCase "customMissing" customMissing
    ]

------------------------------------------------------------------------
-- Instance tests

instanceTests :: [TF.Test]
instanceTests =
  [
    testGroup "Records instances"
    [ testCase "foldr Foldable" (expected @=? F.foldr (:) [] input)
    , testCase "foldl' Foldable" (expected @=? F.foldl' (flip (:)) [] input)
    ]
  ]
  where
    input = S.Cons (Left "empty") (
      S.Cons (Right ("a" :: String)) (S.Nil Nothing BL8.empty))
    expected = ["a" :: String]

------------------------------------------------------------------------
-- Custom conversion option tests

genericConversionTests :: [TF.Test]
genericConversionTests =
    [ testCase "headerOrder" (header ["column1", "column2", "column_3"] @=? hdrs)
    , testCase "encode" (encodeDefaultOrderedByName sampleValues @?= sampleEncoding)
    , testCase "decode" (Right (hdrs, V.fromList sampleValues) @=? decodeByName sampleEncoding)
    , testProperty "roundTrip" rtProp
    ]
  where
    hdrs = headerOrder (undefined :: SampleType)

    sampleValues = [ SampleType ""      1     Nothing
                   , SampleType "field" 99999 (Just 1.234)
                   ]

    sampleEncoding = "column1,column2,column_3\r\n,1,\r\nfield,99999,1.234\r\n"

    rtProp :: [SampleType] -> Bool
    rtProp vs = Right (hdrs, V.fromList vs)
                == decodeByName (encodeDefaultOrderedByName vs)

data SampleType = SampleType
  { _column1  :: !T.Text
  , column2   :: !Int
  , _column_3 :: !(Maybe Double)
  } deriving (Eq, Show, Read, Generic)

sampleOptions :: Options
sampleOptions = defaultOptions { fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = str
    rmUnderscore str       = str

instance ToNamedRecord SampleType where
  toNamedRecord = genericToNamedRecord sampleOptions

instance FromNamedRecord SampleType where
  parseNamedRecord = genericParseNamedRecord sampleOptions

instance DefaultOrdered SampleType where
  headerOrder = genericHeaderOrder sampleOptions

instance Arbitrary SampleType where
  arbitrary = SampleType <$> arbitrary <*> arbitrary <*> arbitrary

------------------------------------------------------------------------
-- Test harness

allTests :: [TF.Test]
allTests = [ testGroup "positional" positionalTests
           , testGroup "named" nameBasedTests
           , testGroup "conversion" conversionTests
           , testGroup "custom-options" customOptionsTests
           , testGroup "instances" instanceTests
           , testGroup "generic-conversions" genericConversionTests
           ]

main :: IO ()
main = defaultMain allTests
