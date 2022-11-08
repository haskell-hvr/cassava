{-# LANGUAGE BangPatterns, CPP, DeriveFunctor, ScopedTypeVariables #-}

-- | This module allows for incremental decoding and encoding of CSV
-- data. This is useful if you e.g. want to interleave I\/O with
-- parsing or if you want finer grained control over how you deal with
-- type conversion errors.
--
-- Decoding example:
--
-- > main :: IO ()
-- > main = withFile "salaries.csv" ReadMode $ \ csvFile -> do
-- >     let loop !_ (Fail _ errMsg) = putStrLn errMsg >> exitFailure
-- >         loop acc (Many rs k)    = loop (acc + sumSalaries rs) =<< feed k
-- >         loop acc (Done rs)      = putStrLn $ "Total salaries: " ++
-- >                                   show (sumSalaries rs + acc)
-- >
-- >         feed k = do
-- >             isEof <- hIsEOF csvFile
-- >             if isEof
-- >                 then return $ k B.empty
-- >                 else k `fmap` B.hGetSome csvFile 4096
-- >     loop 0 (decode NoHeader)
-- >   where
-- >     sumSalaries rs = sum [salary | Right (_ :: String, salary :: Int) <- rs]
--
-- Encoding example:
--
-- > data Person = Person { name   :: !String, salary :: !Int }
-- >     deriving Generic
-- >
-- > instance FromNamedRecord Person
-- > instance ToNamedRecord Person
-- > instance DefaultOrdered Person
-- >
-- > persons :: [Person]
-- > persons = [Person "John" 50000, Person "Jane" 60000]
-- >
-- > main :: IO ()
-- > main = putStrLn $ encodeDefaultOrderedByName (go persons)
-- >   where
-- >     go (x:xs) = encodeNamedRecord x <> go xs
--
module Data.Csv.Incremental
    (
    -- * Decoding
      HeaderParser(..)
    , decodeHeader
    , decodeHeaderWith

    -- $typeconversion
    , Parser(..)

    -- ** Index-based record conversion
    -- $indexbased
    , HasHeader(..)
    , decode
    , decodeWith
    , decodeWithP

    -- ** Name-based record conversion
    -- $namebased
    , decodeByName
    , decodeByNameWith
    , decodeByNameWithP

    -- * Encoding
    -- ** Index-based record conversion
    -- $indexbased
    , encode
    , encodeWith
    , encodeRecord
    , Builder

    -- ** Name-based record conversion
    -- $namebased
    , encodeByName
    , encodeDefaultOrderedByName
    , encodeByNameWith
    , encodeDefaultOrderedByNameWith
    , encodeNamedRecord
    , NamedBuilder
    ) where

import Control.Applicative ((<|>))
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 (endOfInput)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as L
import Data.Semigroup as Semi (Semigroup, (<>))
import qualified Data.Vector as V
import Data.Word (Word8)

import Data.Csv.Conversion hiding (Parser, header, namedRecord, record,
                                   toNamedRecord)
import qualified Data.Csv.Conversion as Conversion
import qualified Data.Csv.Encoding as Encoding
import Data.Csv.Encoding (EncodeOptions(..), Quoting(..), recordSep)
import Data.Csv.Parser
import Data.Csv.Types
import Data.Csv.Util (endOfLine)

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid(mappend, mempty))
import Control.Applicative ((<*))
#endif

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

      -- | The parser parsed and converted zero or more records. Any
      -- records that failed type conversion are returned as @'Left'
      -- errMsg@ and the rest as @'Right' val@. Feed a 'B.ByteString'
      -- to the continuation to continue parsing. Use an 'B.empty'
      -- string to indicate that no more input data is available. If
      -- fed an 'B.empty' string, the continuation is guaranteed to
      -- return either 'Fail' or 'Done'.
    | Many [Either String a] (B.ByteString -> Parser a)

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
    showsPrec d (Many rs _) = showParen (d > appPrec) showStr
      where
        showStr = showString "Many " . showsPrec (appPrec+1) rs .
                  showString " <function>"
    showsPrec d (Done rs) = showParen (d > appPrec) showStr
      where
        showStr = showString "Done " . showsPrec (appPrec+1) rs

-- | Have we read all available input?
data More = Incomplete | Complete
          deriving (Eq, Show)

-- | Efficiently deserialize CSV in an incremental fashion. Equivalent
-- to @'decodeWith' 'defaultDecodeOptions'@.
decode :: FromRecord a
       => HasHeader     -- ^ Data contains header that should be
                        -- skipped
       -> Parser a
decode = decodeWith defaultDecodeOptions

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWith :: FromRecord a
           => DecodeOptions  -- ^ Decoding options
           -> HasHeader      -- ^ Data contains header that should be
                             -- skipped
           -> Parser a
decodeWith !opts hasHeader = decodeWithP parseRecord opts hasHeader

-- | Like 'decodeWith', but lets you pass an explicit parser value instead of
-- using a typeclass
--
-- @since 0.5.2.0
decodeWithP :: (Record -> Conversion.Parser a)
            -> DecodeOptions  -- ^ Decoding options
            -> HasHeader      -- ^ Data contains header that should be
                             -- skipped
            -> Parser a
decodeWithP p !opts hasHeader = case hasHeader of
    HasHeader -> go (decodeHeaderWith opts)
    NoHeader  -> Many [] $ \ s -> decodeWithP' p opts s
  where go (FailH rest msg) = Fail rest msg
        go (PartialH k)     = Many [] $ \ s' -> go (k s')
        go (DoneH _ rest)   = decodeWithP' p opts rest

------------------------------------------------------------------------

-- | Efficiently deserialize CSV in an incremental fashion. The data
-- is assumed to be preceded by a header. Returns a 'HeaderParser'
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
decodeByNameWith !opts = decodeByNameWithP parseNamedRecord opts

-- | Like 'decodeByNameWith', but lets you pass an explicit parser value instead
-- of using a typeclass
--
-- @since 0.5.2.0
decodeByNameWithP :: (NamedRecord -> Conversion.Parser a)
                  -> DecodeOptions  -- ^ Decoding options
                  -> HeaderParser (Parser a)
decodeByNameWithP p !opts = go (decodeHeaderWith opts)
  where
    go (FailH rest msg) = FailH rest msg
    go (PartialH k)     = PartialH $ \ s -> go (k s)
    go (DoneH hdr rest) =
        DoneH hdr (decodeWithP' (p . toNamedRecord hdr) opts rest)

------------------------------------------------------------------------

-- TODO: 'decodeWithP' should probably not take an initial
-- 'B.ByteString' input.

-- | Like 'decode', but lets you customize how the CSV data is parsed.
decodeWithP' :: (Record -> Conversion.Parser a) -> DecodeOptions -> B.ByteString
            -> Parser a
decodeWithP' p !opts = go Incomplete [] . parser
  where
    go !_ !acc (A.Fail rest _ msg)
        | null acc  = Fail rest err
        | otherwise = Many (reverse acc) (\ s -> Fail (rest `B.append` s) err)
      where err = "parse error (" ++ msg ++ ")"
    go Incomplete acc (A.Partial k) = Many (reverse acc) cont
      where cont s = go m [] (k s)
              where m | B.null s  = Complete
                      | otherwise = Incomplete
    go Complete _ (A.Partial _) = moduleError "decodeWithP'" msg
        where msg = "attoparsec should never return Partial in this case"
    go m acc (A.Done rest r)
        | B.null rest = case m of
            Complete   -> Done (reverse acc')
            Incomplete -> Many (reverse acc') (cont [])
        | otherwise   = go m acc' (parser rest)
      where cont acc'' s
                | B.null s  = Done (reverse acc'')
                | otherwise = go Incomplete acc'' (parser s)
            acc' | blankLine r = acc
                 | otherwise   = let !r' = convert r in r' : acc

    parser = A.parse (record (decDelimiter opts) <* (endOfLine <|> endOfInput))
    convert = runParser . p
{-# INLINE decodeWithP' #-}

blankLine :: V.Vector B.ByteString -> Bool
blankLine v = V.length v == 1 && (B.null (V.head v))

------------------------------------------------------------------------
-- * Encoding

-- | Efficiently serialize records in an incremental
-- fashion. Equivalent to @'encodeWith' 'defaultEncodeOptions'@.
encode :: ToRecord a => Builder a -> L.ByteString
encode = encodeWith Encoding.defaultEncodeOptions

-- | Like 'encode', but lets you customize how the CSV data is
-- encoded.
encodeWith :: ToRecord a => EncodeOptions -> Builder a
                 -> L.ByteString
encodeWith opts b =
    Builder.toLazyByteString $
    runBuilder b (encQuoting opts) (encDelimiter opts) (encUseCrLf opts)

-- | Encode a single record.
encodeRecord :: ToRecord a => a -> Builder a
encodeRecord r = Builder $ \ qtng delim useCrLf ->
    Encoding.encodeRecord qtng delim (toRecord r) <> recordSep useCrLf

-- | A builder for building the CSV data incrementally. Just like the
-- @ByteString@ builder, this builder should be used in a
-- right-associative, 'foldr' style. Using '<>' to compose builders in
-- a left-associative, `foldl'` style makes the building not be
-- incremental.
newtype Builder a = Builder {
      runBuilder :: Quoting -> Word8 -> Bool -> Builder.Builder
    }

-- | @since 0.5.0.0
instance Semi.Semigroup (Builder a) where
    Builder f <> Builder g =
        Builder $ \ qtng delim useCrlf ->
        f qtng delim useCrlf <> g qtng delim useCrlf

instance Monoid (Builder a) where
    mempty  = Builder (\ _ _ _ -> mempty)
    mappend = (Semi.<>)

------------------------------------------------------------------------
-- ** Index-based record conversion

-- | Efficiently serialize named records in an incremental fashion,
-- including the leading header. Equivalent to @'encodeWith'
-- 'defaultEncodeOptions'@. The header is written before any records
-- and dictates the field order.
encodeByName :: ToNamedRecord a => Header -> NamedBuilder a -> L.ByteString
encodeByName = encodeByNameWith Encoding.defaultEncodeOptions

-- | Like 'encodeByName', but header and field order is dictated by
-- the 'Conversion.headerOrder' method.
encodeDefaultOrderedByName :: (DefaultOrdered a, ToNamedRecord a) =>
                              NamedBuilder a -> L.ByteString
encodeDefaultOrderedByName =
    encodeDefaultOrderedByNameWith Encoding.defaultEncodeOptions

-- | Like 'encodeByName', but lets you customize how the CSV data is
-- encoded.
encodeByNameWith :: ToNamedRecord a => EncodeOptions -> Header -> NamedBuilder a
                 -> L.ByteString
encodeByNameWith opts hdr b =
    Builder.toLazyByteString $
    encHdr <>
    runNamedBuilder b hdr (encQuoting opts) (encDelimiter opts)
    (encMissing opts) (encUseCrLf opts)
  where
    encHdr
      | encIncludeHeader opts =
          Encoding.encodeRecord (encQuoting opts) (encDelimiter opts) hdr
          <> recordSep (encUseCrLf opts)
      | otherwise = mempty

-- | Like 'encodeDefaultOrderedByName', but lets you customize how the
-- CSV data is encoded.
encodeDefaultOrderedByNameWith ::
    forall a. (DefaultOrdered a, ToNamedRecord a) =>
    EncodeOptions -> NamedBuilder a -> L.ByteString
encodeDefaultOrderedByNameWith opts b =
    Builder.toLazyByteString $
    encHdr <>
    runNamedBuilder b hdr (encQuoting opts)
    (encDelimiter opts) (encMissing opts) (encUseCrLf opts)
  where
    hdr = Conversion.headerOrder (undefined :: a)

    encHdr
      | encIncludeHeader opts =
          Encoding.encodeRecord (encQuoting opts) (encDelimiter opts) hdr
          <> recordSep (encUseCrLf opts)
      | otherwise = mempty

-- | Encode a single named record.
encodeNamedRecord :: ToNamedRecord a => a -> NamedBuilder a
encodeNamedRecord nr = NamedBuilder $ \ hdr qtng delim missing useCrLf ->
    Encoding.encodeNamedRecord hdr qtng delim missing
    (Conversion.toNamedRecord nr) <> recordSep useCrLf

-- | A builder for building the CSV data incrementally. Just like the
-- @ByteString@ builder, this builder should be used in a
-- right-associative, 'foldr' style. Using '<>' to compose builders in
-- a left-associative, `foldl'` style makes the building not be
-- incremental.
newtype NamedBuilder a = NamedBuilder {
      runNamedBuilder :: Header -> Quoting -> Word8 -> (Name -> Field) -> Bool -> Builder.Builder
    }

-- | @since 0.5.0.0
instance Semigroup (NamedBuilder a) where
    NamedBuilder f <> NamedBuilder g =
        NamedBuilder $ \ hdr qtng delim missing useCrlf ->
        f hdr qtng delim missing useCrlf <> g hdr qtng delim missing useCrlf

instance Monoid (NamedBuilder a) where
    mempty = NamedBuilder (\ _ _ _ _ -> mempty)
    mappend = (Semi.<>)

------------------------------------------------------------------------

moduleError :: String -> String -> a
moduleError func msg = error $ "Data.Csv.Incremental." ++ func ++ ": " ++ msg
{-# NOINLINE moduleError #-}
