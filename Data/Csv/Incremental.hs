{-# LANGUAGE BangPatterns, DeriveFunctor #-}

-- | This module allows for incremental decoding of CSV data. This is
-- useful if you e.g. want to interleave I\/O with parsing or if you
-- want finer grained control over how you deal with type conversion
-- errors.
module Data.Csv.Incremental
    (
    -- * Decoding headers
      HeaderParser(..)
    , decodeHeader
    , decodeHeaderWith

    -- * Decoding records
    -- $typeconversion
    , Parser(..)

    -- ** Index-based record conversion
    , decode
    , decodeWith

    -- ** Name-based record conversion
    , decodeByName
    , decodeByNameWith
    ) where

import Control.Applicative
import qualified Data.Attoparsec as A
import Data.Attoparsec.Char8 (endOfLine)
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Data.Csv.Conversion hiding (Parser, Result, record, toNamedRecord)
import qualified Data.Csv.Conversion as Conversion
import Data.Csv.Parser
import Data.Csv.Types

------------------------------------------------------------------------
-- * Decoding headers

-- TODO: Can this be simply be replaced by the 'header' parser?

-- | An incremental parser that when fed data eventually returns a
-- parsed 'Header', or an error.
data HeaderParser a =
      -- | The input data was malformed. The second field contains
      -- information about the parse error.
      FailH !B.ByteString String

      -- | The parser needs more input data before it can produce a
      -- result. Use an 'B.empty' string to indicate that no more
      -- input data is available. After fed an empty string, the
      -- continuation is guaranteed to return either 'FailH' or
      -- 'DoneH'.
    | PartialH (B.ByteString -> HeaderParser a)

      -- | The parse succeeded and produced the given 'Header'.
    | DoneH !Header a
    deriving Functor

instance Show a => Show (HeaderParser a) where
    showsPrec d (FailH rest msg) = showParen (d > appPrec) showStr
      where
        showStr = showString "FailH " . showsPrec (appPrec+1) rest .
                  showString " " . showsPrec (appPrec+1) msg
    showsPrec _ (PartialH _) = showString "PartialH <function>"
    showsPrec d (DoneH hdr x) = showParen (d > appPrec) showStr
      where
        showStr = showString "DoneH " . showsPrec (appPrec+1) hdr .
                  showString " " . showsPrec (appPrec+1) x

-- Application has precedence one more than the most tightly-binding
-- operator
appPrec :: Int
appPrec = 10

-- | Parse a CSV header in an incremental fashion. When done, the
-- 'HeaderParser' returns any unconsumed input in its second field.
decodeHeader :: B.ByteString -> HeaderParser B.ByteString
decodeHeader = decodeHeaderWith defaultDecodeOptions

-- | Like 'decodeHeader', but lets you customize how the CSV data is
-- parsed.
decodeHeaderWith :: DecodeOptions -> B.ByteString -> HeaderParser B.ByteString
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

-- $typeconversion
--
-- Just like in the case of non-incremental decoding, there are two
-- ways to convert CSV records to and from and user-defined data
-- types: index-based conversion and name-based conversion.

-- | An incremental parser that when fed data eventually produces some
-- parsed records, converted to the desired type, or an error in case
-- of malformed input data.
data Parser a =
    -- | The input data was malformed. The second field contains
    -- information about the parse error.
      Fail !B.ByteString String

      -- | The parser needs more input data before it can produce a
      -- result. Use an 'B.empty' string to indicate that no more
      -- input data is available. After fed an empty string, the
      -- continuation is guaranteed to return either 'Fail' or 'Done'.
    | Partial (B.ByteString -> Parser a)

      -- | The parser parsed and converted some records. Any records
      -- that failed type conversion are returned as @'Left' errMsg@
      -- and the rest as @'Right' val@. Feed a 'B.ByteString' to the
      -- continuation to continue parsing. Use an 'B.empty' string to
      -- indicate that no more input data is available. After fed an
      -- empty string, the continuation is guaranteed to return either
      -- 'Fail' or 'Done'.
    | Some [Either String a] (B.ByteString -> Parser a)

      -- | The parser parsed and converted some records. Any records
      -- that failed type conversion are returned as @'Left' errMsg@
      -- and the rest as @'Right' val@.
    | Done [Either String a]
    deriving Functor

instance Show a => Show (Parser a) where
    showsPrec d (Fail rest msg) = showParen (d > appPrec) showStr
      where
        showStr = showString "Fail " . showsPrec (appPrec+1) rest .
                  showString " " . showsPrec (appPrec+1) msg
    showsPrec _ (Partial _) = showString "Partial <function>"
    showsPrec d (Some rs _) = showParen (d > appPrec) showStr
      where
        showStr = showString "Some " . showsPrec (appPrec+1) rs .
                  showString " <function>"
    showsPrec d (Done rs) = showParen (d > appPrec) showStr
      where
        showStr = showString "Done " . showsPrec (appPrec+1) rs

-- | Have we read all available input?
data More = Incomplete | Complete
          deriving (Eq, Show)

-- | Efficiently deserialize CSV in an incremental fashion. Equivalent
-- to @'decodeByNameWith' 'defaultDecodeOptions'@.
decode :: FromRecord a => B.ByteString -> Parser a
decode = decodeWith defaultDecodeOptions

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a => DecodeOptions -> B.ByteString -> Parser a
decodeWith = decodeWithP parseRecord

------------------------------------------------------------------------

-- | Efficiently deserialize CSV in an incremental fashion. The data
-- is assumed to be preceeded by a header. Returns a 'HeaderParser'
-- that when done produces a 'Parser' for parsing the actual records.
-- Equivalent to @'decodeByNameWith' 'defaultDecodeOptions'@.
decodeByName :: FromNamedRecord a => B.ByteString -> HeaderParser (Parser a)
decodeByName = decodeByNameWith defaultDecodeOptions

-- | Like 'decodeByName', but lets you customize how the CSV data is
-- parsed.
decodeByNameWith :: FromNamedRecord a => DecodeOptions -> B.ByteString
                 -> HeaderParser (Parser a)
decodeByNameWith !opts = runParser . decodeHeaderWith opts
  where
    runParser (FailH rest msg) = FailH rest msg
    runParser (PartialH k)     = PartialH $ \ s -> runParser (k s)
    runParser (DoneH hdr rest) =
        DoneH hdr (decodeWithP (parseNamedRecord . toNamedRecord hdr) opts rest)

-- Copied from Data.Csv.Parser
toNamedRecord :: Header -> Record -> NamedRecord
toNamedRecord hdr v = HM.fromList . V.toList $ V.zip hdr v

------------------------------------------------------------------------

-- TODO: Call endOfLine parser once more when receiving empty input?

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWithP :: (Record -> Conversion.Parser a) -> DecodeOptions -> B.ByteString
            -> Parser a
decodeWithP p !opts = go Incomplete [] . initialParser
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
    convert = parseEither . p
{-# INLINE decodeWithP #-}

blankLine :: V.Vector B.ByteString -> Bool
blankLine v = V.length v == 1 && (B.null (V.head v))

moduleError :: String -> String -> a
moduleError func msg = error $ "Data.Csv.Incremental." ++ func ++ ": " ++ msg
{-# NOINLINE moduleError #-}
