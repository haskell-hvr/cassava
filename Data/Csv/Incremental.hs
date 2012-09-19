{-# LANGUAGE BangPatterns, DeriveFunctor #-}

-- | This module allows for incremental decoding of CSV data. This is
-- useful if you e.g. want to interleave I\/O with parsing or if you
-- want finer grained control over how you deal with type conversion
-- errors.
module Data.Csv.Incremental
    (
    -- * Decoding headers
      decodeHeader
    , decodeHeaderWith
    , HeaderParser(..)

    -- * Decoding records
    , decode
    , decodeWith
    , Parser(..)
    ) where

import Control.Applicative
import qualified Data.Attoparsec as A
import Data.Attoparsec.Char8 (endOfLine)
import qualified Data.ByteString as B
import qualified Data.Vector as V
import Text.Show.Functions ()

import Data.Csv.Conversion hiding (Parser, Result, record)
import Data.Csv.Parser
import Data.Csv.Types

------------------------------------------------------------------------
-- * Decoding headers

-- TODO: Write custom Show instance to avoid re-exporting orphan
-- instance.

-- TODO: add more failure info

-- | A parser that when fed data eventually returns a parsed 'Header',
-- or an error.
data HeaderParser =
      -- | The input data was malformed. The second fields contains
      -- information about the parse error.
      FailH !B.ByteString String

      -- | The parser needs more input data before it can produce a
      -- result. Use an 'B.empty' string to indicate that no more
      -- input data is available. After fed an empty string, the
      -- continuation is guaranteed to return either 'FailH' or
      -- 'DoneH'.
    | PartialH (B.ByteString -> HeaderParser)

      -- | The parse succeeded and produced the given 'Header'. The
      -- second field contains any unconsumed input.
    | DoneH !Header !B.ByteString
    deriving Show

-- | Create parser for decoding a CSV header.
decodeHeader :: B.ByteString -> HeaderParser
decodeHeader = decodeHeaderWith defaultDecodeOptions

-- | Like 'decodeHeader', but lets you customize how the CSV data is
-- parsed.
decodeHeaderWith :: DecodeOptions -> B.ByteString -> HeaderParser
decodeHeaderWith !opts = go . parser
  where
    parser = A.parse (header $ decDelimiter opts)

    go (A.Fail rest _ msg) = FailH rest err
      where err = "parse error (" ++ msg ++ ")"
    -- TODO: Check empty and give attoparsec one last chance to return
    -- something:
    go (A.Partial k)       = PartialH $ \ s -> go (k s)
    go (A.Done rest r)     = DoneH r rest

------------------------------------------------------------------------
-- * Decoding records

-- | A parser that when fed data eventually produces some parsed
-- records, converted to the desired type, or an error in case of
-- malformed input data.
data Parser a =
    -- | The input data was malformed. The second fields contains
    -- information about the parse error.
      Fail !B.ByteString String

      -- | The parser needs more input data before it can produce a
      -- result. Use an 'B.empty' string to indicate that no more
      -- input data is available. After fed an empty string, the
      -- continuation is guaranteed to return either 'Fail' or 'Done'.
    | Partial (B.ByteString -> Parser a)

      -- | The parser parsed and converted some records. Any records
      -- that failed type conversion are returned as @'Left' err@ and
      -- the rest as @'Right' val@. Feed a 'B.ByteString' to the
      -- continuation to continue parsing. Use an 'B.empty' string to
      -- indicate that no more input data is available. After fed an
      -- empty string, the continuation is guaranteed to return either
      -- 'Fail' or 'Done'.
    | Some [Either String a] (B.ByteString -> Parser a)

      -- | The parser parsed and converted some records. Any records
      -- that failed type conversion are returned as @'Left' err@ and
      -- the rest as @'Right' val@.
    | Done [Either String a]
    deriving (Functor, Show)

-- | Have we read all available input?
data More = Incomplete | Complete
          deriving (Eq, Show)

-- | Create parser for decoding CSV records.
decode :: FromRecord a => B.ByteString -> Parser a
decode = decodeWith defaultDecodeOptions

-- TODO: Call endOfLine parser once more when receiving empty input?

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a => DecodeOptions -> B.ByteString -> Parser a
decodeWith !opts = go Incomplete [] . initialParser
  where
    go !_ !acc (A.Fail rest _ msg)
        | null acc  = Fail rest err
        | otherwise = Some (reverse acc) (\ s -> Fail (rest `B.append` s) err)
      where err = "parse error (" ++ msg ++ ")"
    go Incomplete acc (A.Partial k)
        | null acc  = Partial cont
        | otherwise = Some (reverse acc) cont
      where cont s = go m [] (k s)
              where m | B.null s  = Complete
                      | otherwise = Incomplete
    go Complete _ (A.Partial _) = moduleError "decodeWith" msg
        where msg = "attoparsec should never return Partial in this case"
    go m acc (A.Done rest r)
        | B.null rest = case m of
            Complete   -> Done (reverse acc')
            Incomplete -> Partial cont
        | otherwise   = go m acc' (parser rest)
      where cont s = go m' acc' (parser s)
              where m' | B.null s  = Complete
                       | otherwise = Incomplete
            acc' | blankLine r = acc
                 | otherwise   = convert r : acc

    initialParser = A.parse (record (decDelimiter opts))
    parser = A.parse (endOfLine *> record (decDelimiter opts))
    convert = parseEither . parseRecord

blankLine :: V.Vector B.ByteString -> Bool
blankLine v = V.length v == 1 && (B.null (V.head v))

moduleError :: String -> String -> a
moduleError func msg = error $ "Data.Csv.Incremental." ++ func ++ ": " ++ msg
{-# NOINLINE moduleError #-}
