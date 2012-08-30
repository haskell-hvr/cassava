module Data.Csv.Streaming
    ( decode
    , Result(..)
    ) where

import qualified Data.Attoparsec as A
import qualified Data.ByteString as B

import Data.Csv.Conversion hiding (Result)
import Data.Csv.Parser

data Result a = Some [a] (B.ByteString -> Result a)
              | Partial (B.ByteString -> Result a)
              | Fail B.ByteString String -- TODO: add more failure info

decode :: FromRecord a => B.ByteString -> Result a
decode = go
  where
    go s = case A.parse (csv defaultDecodeOptions) s of
        A.Fail rest _ msg -> Fail rest $ "parse error (" ++ msg ++ ")"
        A.Partial k       -> Partial go
        A.Done rest r     -> Some undefined undefined
