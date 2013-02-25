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
    -- ** Providing input
    -- $feed-header
    , feedChunkH
    , feedEndOfInputH

    -- * Decoding records
    -- $typeconversion
    , Parser(..)

    -- ** Index-based record conversion
    -- $indexbased
    , decode
    , decodeWith

    -- ** Name-based record conversion
    -- $namebased
    , decodeByName
    , decodeByNameWith

    -- ** Providing input
    -- $feed-records
    , feedChunk
    , feedEndOfInput
    ) where

import Control.Applicative ((<*), (<|>))
import qualified Data.Attoparsec as A
import Data.Attoparsec.Char8 (endOfInput, endOfLine)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import Data.Csv.Conversion hiding (Parser, record, toNamedRecord)
import qualified Data.Csv.Conversion as Conversion
import Data.Csv.Parser
import Data.Csv.Types

-- $feed-header
--
-- These functions are sometimes convenient when working with
-- 'HeaderParser', but don't let you do anything you couldn't already
-- do using the 'HeaderParser' constructors directly.

-- $indexbased
--
-- See documentation on index-based conversion in "Data.Csv" for more
-- information.

-- $namebased
--
-- See documentation on name-based conversion in "Data.Csv" for more
-- information.

-- $feed-records
--
-- These functions are sometimes convenient when working with
-- 'Parser', but don't let you do anything you couldn't already do
-- using the 'Parser' constructors directly.

------------------------------------------------------------------------
-- * Decoding headers

-- | An incremental parser that when fed data eventually returns a
-- parsed 'Header', or an error.
data HeaderParser a =
      -- | The input data was malformed. The first field contains any
      -- unconsumed input and second field contains information about
      -- the parse error.
      FailH !B.ByteString String

      -- | The parser needs more input data before it can produce a
      -- result. Use an 'B.empty' string to indicate that no more
      -- input data is available. If fed an 'B.empty string', the
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

-- | Feed a 'HeaderParser' with more input. If the 'HeaderParser' is
-- 'FailH' it will add the input to 'B.ByteString' of unconsumed
-- input. If the 'HeaderParser' is 'DoneH' it will drop the extra
-- input on the floor.
feedChunkH :: HeaderParser a -> B.ByteString -> HeaderParser a
feedChunkH (FailH rest err) s = FailH (B.append rest s) err
feedChunkH (PartialH k) s     = k s
feedChunkH d@(DoneH _ _) _s   = d

-- | Tell a 'HeaderParser' that there is no more input. This passes
-- 'B.empty' to a 'PartialH' parser, otherwise returns the parser
-- unchanged.
feedEndOfInputH :: HeaderParser a -> HeaderParser a
feedEndOfInputH (PartialH k) = k B.empty
feedEndOfInputH p            = p

-- | Parse a CSV header in an incremental fashion. When done, the
-- 'HeaderParser' returns any unconsumed input in the second field of
-- the 'DoneH' constructor.
decodeHeader :: HeaderParser B.ByteString
decodeHeader = decodeHeaderWith defaultDecodeOptions

-- | Like 'decodeHeader', but lets you customize how the CSV data is
-- parsed.
decodeHeaderWith :: DecodeOptions -> HeaderParser B.ByteString
decodeHeaderWith !opts = PartialH (go . parser)
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
      -- | The input data was malformed. The first field contains any
      -- unconsumed input and second field contains information about
      -- the parse error.
      Fail !B.ByteString String

      -- | The parser needs more input data before it can produce a
      -- result. Use an 'B.empty' string to indicate that no more
      -- input data is available. If fed an 'B.empty' string, the
      -- continuation is guaranteed to return either 'Fail' or 'Done'.
    | Partial (B.ByteString -> Parser a)

      -- | The parser parsed and converted some records. Any records
      -- that failed type conversion are returned as @'Left' errMsg@
      -- and the rest as @'Right' val@. Feed a 'B.ByteString' to the
      -- continuation to continue parsing. Use an 'B.empty' string to
      -- indicate that no more input data is available. If fed an
      -- 'B.empty' string, the continuation is guaranteed to return
      -- either 'Fail' or 'Done'.
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

-- | Feed a 'Parser' with more input. If the 'Parser' is 'Fail' it
-- will add the input to 'B.ByteString' of unconsumed input. If the
-- 'Parser' is 'Done' it will drop the extra input on the floor.
feedChunk :: Parser a -> B.ByteString -> Parser a
feedChunk (Fail rest err) s = Fail (B.append rest s) err
feedChunk (Partial k) s     = k s
feedChunk (Some xs k) s     = Some xs (\ s' -> k s `feedChunk` s')
feedChunk (Done xs) _s      = Done xs

-- | Tell a 'Parser' that there is no more input. This passes 'empty'
-- to a 'Partial' parser, otherwise returns the parser unchanged.
feedEndOfInput :: Parser a -> Parser a
feedEndOfInput (Partial k)     = k B.empty
feedEndOfInput p               = p

-- | Have we read all available input?
data More = Incomplete | Complete
          deriving (Eq, Show)

-- | Efficiently deserialize CSV in an incremental fashion. Equivalent
-- to @'decodeWith' 'defaultDecodeOptions'@.
decode :: FromRecord a
       => Bool          -- ^ Data contains header that should be
                        -- skipped
       -> Parser a
decode = decodeWith defaultDecodeOptions

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a
           => DecodeOptions  -- ^ Decoding options
           -> Bool           -- ^ Data contains header that should be
                             -- skipped
           -> Parser a
decodeWith !opts skipHeader
    | skipHeader = Partial $ \ s -> go (decodeHeaderWith opts `feedChunkH` s)
    | otherwise  = Partial (decodeWithP parseRecord opts)
  where go (FailH rest msg) = Fail rest msg
        go (PartialH k)     = Partial $ \ s' -> go (k s')
        go (DoneH _ rest)   = decodeWithP parseRecord opts rest

------------------------------------------------------------------------

-- | Efficiently deserialize CSV in an incremental fashion. The data
-- is assumed to be preceeded by a header. Returns a 'HeaderParser'
-- that when done produces a 'Parser' for parsing the actual records.
-- Equivalent to @'decodeByNameWith' 'defaultDecodeOptions'@.
decodeByName :: FromNamedRecord a
             => HeaderParser (Parser a)
decodeByName = decodeByNameWith defaultDecodeOptions

-- | Like 'decodeByName', but lets you customize how the CSV data is
-- parsed.
decodeByNameWith :: FromNamedRecord a
                 => DecodeOptions  -- ^ Decoding options
                 -> HeaderParser (Parser a)
decodeByNameWith !opts =
    PartialH (go . (decodeHeaderWith opts `feedChunkH`))
  where
    go (FailH rest msg) = FailH rest msg
    go (PartialH k)     = PartialH $ \ s -> go (k s)
    go (DoneH hdr rest) =
        DoneH hdr (decodeWithP (parseNamedRecord . toNamedRecord hdr) opts rest)

------------------------------------------------------------------------

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWithP :: (Record -> Conversion.Parser a) -> DecodeOptions -> B.ByteString
            -> Parser a
decodeWithP p !opts = go Incomplete [] . parser
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
    go Complete _ (A.Partial _) = moduleError "decodeWithP" msg
        where msg = "attoparsec should never return Partial in this case"
    go m acc (A.Done rest r)
        | B.null rest = case m of
            Complete   -> Done (reverse acc')
            Incomplete
                | null acc' -> Partial (cont acc')
                | otherwise -> Some (reverse acc') (cont [])
        | otherwise   = go m acc' (parser rest)
      where cont acc'' s
                | B.null s  = Done (reverse acc'')
                | otherwise = go Incomplete acc'' (parser s)
            acc' | blankLine r = acc
                 | otherwise   = let !r' = convert r in r' : acc

    parser = A.parse (record (decDelimiter opts) <* (endOfLine <|> endOfInput))
    convert = runParser . p
{-# INLINE decodeWithP #-}

blankLine :: V.Vector B.ByteString -> Bool
blankLine v = V.length v == 1 && (B.null (V.head v))

moduleError :: String -> String -> a
moduleError func msg = error $ "Data.Csv.Incremental." ++ func ++ ": " ++ msg
{-# NOINLINE moduleError #-}
