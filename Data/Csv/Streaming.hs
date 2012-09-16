{-# LANGUAGE BangPatterns, DeriveFunctor #-}

module Data.Csv.Streaming
    (
    -- * Continuation based parsing
      decode
    , decodeWith
    , Parser(..)
    , RecordParser(..)

    -- * Lazy parsing
    , RecordStream(..)
    , Result(..)
    , lazyDecode
    , lazyDecodeWith
    ) where

import Control.Applicative
import qualified Data.Attoparsec as A
import Data.Attoparsec.Char8 (endOfLine)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Text.Show.Functions ()

import Data.Csv.Conversion hiding (Parser, Result, record)
import Data.Csv.Parser
import Data.Csv.Types

------------------------------------------------------------------------
-- * Continuation based parsing

data Parser a = FailH B.ByteString String -- TODO: add more failure info
              | PartialH (B.ByteString -> Parser a)
              | DoneH Header (RecordParser a)
              deriving Show

data RecordParser a = Fail B.ByteString String -- TODO: add more failure info
                    | Partial (B.ByteString -> RecordParser a)
                    | Some a (B.ByteString -> RecordParser a)
                    | Done a
                    deriving Show

-- TODO: Distinguish between failing to parse and failing to convert.
-- The latter is recoverable.
decode :: FromRecord a => B.ByteString -> Parser [Either String a]
decode = decodeWith defaultDecodeOptions

decodeWith :: FromRecord a => DecodeOptions -> B.ByteString
           -> Parser [Either String a]
decodeWith opts = go . parser
  where
   parser = A.parse (header $ decDelimiter opts)

   go (A.Fail rest _ msg) = FailH rest err
     where err = "parse error (" ++ msg ++ ")"
   -- TODO: Check empty and give attoparsec one last chance to return
   -- something:
   go (A.Partial k)       = PartialH $ \ s -> go (k s)
   go (A.Done rest r)     = DoneH r (decodeRecordsWith opts rest)

-- | Have we read all available input?
data More = Incomplete | Complete
          deriving (Eq, Show)

-- TODO: Do we want to expose this?
-- TODO: Call endOfLine parser once more when receiving empty input?
decodeRecordsWith :: FromRecord a => DecodeOptions -> B.ByteString
                  -> RecordParser [Either String a]
decodeRecordsWith opts = go Incomplete [] . initialParser
  where
    go !_ acc (A.Fail rest _ msg)
        | null acc  = Fail rest err
        | otherwise = Some (reverse acc) (\ s -> Fail (rest `B.append` s) err)
      where err = "parse error (" ++ msg ++ ")"
    go Incomplete acc (A.Partial k)
        | null acc  = Partial cont
        | otherwise = Some (reverse acc) cont
      where cont s = go m [] (k s)
              where m | B.null s  = Complete
                      | otherwise = Incomplete
    go Complete _ (A.Partial _) = moduleError "decodeRecordsWith" msg
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
moduleError func msg = error $ "Data.Csv.Streaming." ++ func ++ ": " ++ msg
{-# NOINLINE moduleError #-}

------------------------------------------------------------------------
-- * Lazy parsing

-- TODO: Check what lazyDecode ... :: Result String means. Overlapping
-- instance?

data RecordStream a = Nil BL.ByteString
                    | FailS BL.ByteString String
                    | Cons a (RecordStream a)
                    deriving (Eq, Functor, Show)

data Result a = FailR BL.ByteString String
              | DoneR Header (RecordStream (Either String a))
              deriving (Eq, Functor, Show)

lazyDecode :: FromRecord a => BL.ByteString -> Result a
lazyDecode = lazyDecodeWith defaultDecodeOptions

lazyDecodeWith :: FromRecord a => DecodeOptions -> BL.ByteString -> Result a
lazyDecodeWith opts s0 = go (BL.toChunks s0) (decodeWith opts B.empty)
  where
    go ss (FailH rest err) = FailR (BL.fromChunks (rest:ss)) err
    go [] (PartialH k)     = go [] (k B.empty)
    go (s:ss) (PartialH k) = go ss (k s)
    go ss (DoneH hdr p)    = DoneR hdr (go2 ss p)

    go2 ss (Done xs)       = foldr Cons (Nil (BL.fromChunks ss)) xs  -- Can there be a non-empty ss here?
    go2 ss (Fail rest err) = FailS (BL.fromChunks (rest:ss)) err
    go2 [] (Partial k)     = go2 [] (k B.empty)
    go2 (s:ss) (Partial k) = go2 ss (k s)
    go2 [] (Some xs k)     = foldr Cons (go2 [] (k B.empty)) xs
    go2 (s:ss) (Some xs k) = foldr Cons (go2 ss (k s)) xs