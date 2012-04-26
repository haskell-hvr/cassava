module Data.Ceason.Parser.Internal
    ( csv
    , csvWithHeader
    , header
    , record
    , field
    , decodeWith
    , decodeWithEither
    ) where

import Blaze.ByteString.Builder (fromByteString, toByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import Control.Applicative
import Data.Attoparsec.Char8 hiding (Parser, Result, parse)
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Lazy as AL
import qualified Data.Attoparsec.Zepto as Z
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Unsafe as S
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import qualified Data.Vector as V
import Data.Word

import Data.Ceason.Types hiding (record, toNamedRecord)

csv :: AL.Parser Csv
csv = do
    vals <- record `sepBy1` endOfLine
    _ <- optional endOfLine
    endOfInput
    return (V.fromList (removeBlankLines vals))

-- TODO: Rename headers to header everywhere.
csvWithHeader :: AL.Parser (Header, V.Vector NamedRecord)
csvWithHeader = do
    hdr <- header
    vals <- map (toNamedRecord hdr) . removeBlankLines <$>
            record `sepBy1` endOfLine
    _ <- optional endOfLine
    endOfInput
    return (hdr, V.fromList vals)

toNamedRecord :: V.Vector S.ByteString -> Record -> NamedRecord
toNamedRecord hdr v = HM.fromList . V.toList $ V.zip hdr v

-- | Parse a CSV header line, including the terminating line
-- separator.
header :: AL.Parser Header
header = V.fromList <$> name `sepBy1` comma <* endOfLine

name :: AL.Parser Field  -- TODO: Create Name type alias
name = field

removeBlankLines :: [Record] -> [Record]
removeBlankLines = filter (not . blankLine)
  where blankLine v = V.length v == 1 && (S.null (V.head v))

record :: AL.Parser Record
record = V.fromList <$> field `sepBy1` comma

field :: AL.Parser Field
field = do
    mb <- A.peekWord8
    -- We purposely don't use <|> as we want to commit to the first
    -- choice if we see a double quote.
    case mb of
        Just b | b == doubleQuote -> escapedField
        _                         -> unescapedField

escapedField :: AL.Parser S.ByteString
escapedField = do
    _ <- dquote
    -- The scan state is 'True' if the previous character was a double
    -- quote.  We need to drop a trailing double quote left by scan.
    s <- S.init <$> (A.scan False $ \s c -> if c == doubleQuote
                                            then if s then Just False
                                                 else Just True
                                            else if s then Nothing
                                                 else Just False)
    if doubleQuote `S.elem` s
        then case Z.parse unescape s of
            Right r  -> return r
            Left err -> fail err
        else return s

-- TODO: Perhaps allow all possible bytes (except , and ").
unescapedField :: AL.Parser S.ByteString
unescapedField = A.takeWhile isTextdata
  where
    isTextdata :: Word8 -> Bool
    isTextdata c = c >= 0x20 && c <= 0x21 ||
                   c >= 0x23 && c <= 0x2b ||
                   c >= 0x2d && c <= 0x7e

comma, dquote :: AL.Parser Char
comma = char ','
dquote = char '"'

unescape :: Z.Parser S.ByteString
unescape = toByteString <$> go mempty where
  go acc = do
    h <- Z.takeWhile (/= doubleQuote)
    let rest = do
          start <- Z.take 2
          if (S.unsafeHead start == doubleQuote &&
              S.unsafeIndex start 1 == doubleQuote)
              then go (acc `mappend` fromByteString h `mappend` fromChar '"')
              else fail "invalid CSV escape sequence"
    done <- Z.atEnd
    if done
      then return (acc `mappend` fromByteString h)
      else rest

doubleQuote :: Word8
doubleQuote = 34

decodeWith :: AL.Parser a -> (a -> Result b) -> L.ByteString -> Maybe b
decodeWith p to s =
    case AL.parse p s of
      AL.Done _ v -> case to v of
          Success a -> Just a
          _         -> Nothing
      _           -> Nothing
{-# INLINE decodeWith #-}

decodeWithEither :: AL.Parser a -> (a -> Result b) -> L.ByteString -> Either String b
decodeWithEither p to s =
    case AL.parse p s of
      AL.Done _ v -> case to v of
          Success a -> Right a
          Error msg -> Left msg
      AL.Fail s _ msg  -> Left $ msg ++ " at: " ++ BL8.unpack s
{-# INLINE decodeWithEither #-}
