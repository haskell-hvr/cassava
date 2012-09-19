{-# LANGUAGE BangPatterns, DeriveFunctor #-}

module Data.Csv.Incremental
    (
    -- * Continuation based parsing
      decodeHeader
    , decodeHeaderWith
    , HeaderParser(..)
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
-- * Continuation based parsing

-- TODO: Write custom Show instance to avoid re-exporting orphan
-- instance.

data HeaderParser = FailH B.ByteString String -- TODO: add more failure info
                  | PartialH (B.ByteString -> HeaderParser)
                  | DoneH Header B.ByteString
                  deriving Show

decodeHeader :: B.ByteString -> HeaderParser
decodeHeader = decodeHeaderWith defaultDecodeOptions

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

data Parser a = Fail B.ByteString String -- TODO: add more failure info
              | Partial (B.ByteString -> Parser a)
              | Some [Either String a] (B.ByteString -> Parser a)
              | Done [Either String a]
              deriving Show

------------------------------------------------------------------------

-- | Have we read all available input?
data More = Incomplete | Complete
          deriving (Eq, Show)

decode :: FromRecord a => B.ByteString -> Parser a
decode = decodeWith defaultDecodeOptions

-- TODO: Call endOfLine parser once more when receiving empty input?
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
