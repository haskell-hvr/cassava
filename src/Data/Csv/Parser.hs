{-# LANGUAGE BangPatterns, CPP #-}

-- | A CSV parser. The parser defined here is RFC 4180 compliant, with
-- the following extensions:
--
--  * Empty lines are ignored.
--
--  * Non-escaped fields may contain any characters except
--    double-quotes, commas, carriage returns, and newlines.
--
--  * Escaped fields may contain any characters (but double-quotes
--    need to be escaped).
--
-- The functions in this module can be used to implement e.g. a
-- resumable parser that is fed input incrementally.
module Data.Csv.Parser
    ( DecodeOptions(..)
    , defaultDecodeOptions
    , csv
    , csvWithHeader
    , header
    , record
    , name
    , field
    ) where

import Data.ByteString.Builder (byteString, toLazyByteString, charUtf8)
import Control.Applicative (optional)
import Data.Attoparsec.ByteString.Char8 (char, endOfInput)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as S
import qualified Data.Vector as V
import Data.Word (Word8)

import Data.Csv.Types
import Data.Csv.Util ((<$!>), blankLine, endOfLine, liftM2', cr, newline, doubleQuote, toStrict)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (*>), (<*), pure)
import Data.Monoid (mappend, mempty)
#endif

-- | Options that controls how data is decoded. These options can be
-- used to e.g. decode tab-separated data instead of comma-separated
-- data.
--
-- To avoid having your program stop compiling when new fields are
-- added to 'DecodeOptions', create option records by overriding
-- values in 'defaultDecodeOptions'. Example:
--
-- > myOptions = defaultDecodeOptions {
-- >       decDelimiter = fromIntegral (ord '\t')
-- >     }
data DecodeOptions = DecodeOptions
    { -- | Field delimiter.
      decDelimiter  :: {-# UNPACK #-} !Word8
    } deriving (Eq, Show)

-- | Decoding options for parsing CSV files.
defaultDecodeOptions :: DecodeOptions
defaultDecodeOptions = DecodeOptions
    { decDelimiter = 44  -- comma
    }

-- | Parse a CSV file that does not include a header.
csv :: DecodeOptions -> AL.Parser Csv
csv !opts = do
    vals <- sepByEndOfLine1' (record (decDelimiter opts))
    _ <- optional endOfLine
    endOfInput
    let nonEmpty = removeBlankLines vals
    return $! V.fromList nonEmpty
{-# INLINE csv #-}

-- | Specialized version of 'sepBy1'' which is faster due to not
-- accepting an arbitrary separator.
sepByDelim1' :: AL.Parser a
             -> Word8  -- ^ Field delimiter
             -> AL.Parser [a]
sepByDelim1' p !delim = liftM2' (:) p loop
  where
    loop = do
        mb <- A.peekWord8
        case mb of
            Just b | b == delim -> liftM2' (:) (A.anyWord8 *> p) loop
            _                   -> pure []
{-# INLINE sepByDelim1' #-}

-- | Specialized version of 'sepBy1'' which is faster due to not
-- accepting an arbitrary separator.
sepByEndOfLine1' :: AL.Parser a
                 -> AL.Parser [a]
sepByEndOfLine1' p = liftM2' (:) p loop
  where
    loop = do
        mb <- A.peekWord8
        case mb of
            Just b | b == cr ->
                liftM2' (:) (A.anyWord8 *> A.word8 newline *> p) loop
                   | b == newline ->
                liftM2' (:) (A.anyWord8 *> p) loop
            _ -> pure []
{-# INLINE sepByEndOfLine1' #-}

-- | Parse a CSV file that includes a header.
csvWithHeader :: DecodeOptions -> AL.Parser (Header, V.Vector NamedRecord)
csvWithHeader !opts = do
    !hdr <- header (decDelimiter opts)
    vals <- map (toNamedRecord hdr) . removeBlankLines <$>
            sepByEndOfLine1' (record (decDelimiter opts))
    _ <- optional endOfLine
    endOfInput
    let !v = V.fromList vals
    return (hdr, v)

-- | Parse a header, including the terminating line separator.
header :: Word8  -- ^ Field delimiter
       -> AL.Parser Header
header !delim = V.fromList <$!> name delim `sepByDelim1'` delim <* endOfLine

-- | Parse a header name. Header names have the same format as regular
-- 'field's.
name :: Word8 -> AL.Parser Name
name !delim = field delim

removeBlankLines :: [Record] -> [Record]
removeBlankLines = filter (not . blankLine)

-- | Parse a record, not including the terminating line separator. The
-- terminating line separate is not included as the last record in a
-- CSV file is allowed to not have a terminating line separator. You
-- most likely want to use the 'endOfLine' parser in combination with
-- this parser.
record :: Word8  -- ^ Field delimiter
       -> AL.Parser Record
record !delim = V.fromList <$!> field delim `sepByDelim1'` delim
{-# INLINE record #-}

-- | Parse a field. The field may be in either the escaped or
-- non-escaped format. The return value is unescaped.
field :: Word8 -> AL.Parser Field
field !delim = do
    mb <- A.peekWord8
    -- We purposely don't use <|> as we want to commit to the first
    -- choice if we see a double quote.
    case mb of
        Just b | b == doubleQuote -> escapedField
        _                         -> unescapedField delim
{-# INLINE field #-}

escapedField :: AL.Parser S.ByteString
escapedField = do
    _ <- dquote
    -- The scan state is 'True' if the previous character was a double
    -- quote.  We need to drop a trailing double quote left by scan.
    s <- S.init <$> (A.scan False $ \s c -> if c == doubleQuote
                                            then Just (not s)
                                            else if s then Nothing
                                                 else Just False)
    if doubleQuote `S.elem` s
        then case Z.parse unescape s of
            Right r  -> return r
            Left err -> fail err
        else return s

unescapedField :: Word8 -> AL.Parser S.ByteString
unescapedField !delim = A.takeWhile (\ c -> c /= doubleQuote &&
                                            c /= newline &&
                                            c /= delim &&
                                            c /= cr)

dquote :: AL.Parser Char
dquote = char '"'

unescape :: Z.Parser S.ByteString
unescape = (toStrict . toLazyByteString) <$!> go mempty where
  go acc = do
    h <- Z.takeWhile (/= doubleQuote)
    let rest = do
          start <- Z.take 2
          if (S.unsafeHead start == doubleQuote &&
              S.unsafeIndex start 1 == doubleQuote)
              then go (acc `mappend` byteString h `mappend` charUtf8 '"')
              else fail "invalid CSV escape sequence"
    done <- Z.atEnd
    if done
      then return (acc `mappend` byteString h)
      else rest
