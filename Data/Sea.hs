-- | A RFC 4180 compliant CSV parsing and encodig module.
module Data.Sea where

import Control.Applicative
import Data.Attoparsec.Char8
import qualified Data.Attoparsec.Lazy as L
import Data.Vector as Vector hiding ((++))

decode s = case L.parse csv s of
    L.Done _ v -> Just v
    _          -> Nothing

csv = do
    vals <- record `sepBy` endOfLine
    optional endOfLine
    endOfInput
    return (Vector.fromList vals)

record = Vector.fromList <$> field `sepBy` comma

field = do
    c <- anyChar
    case c of
        '"' -> escapedField
        _   -> unescapedField
    
escapedField = undefined
unescapedField = takeTill (\ c -> c == ',' || c == '\r' || c == '\n')

comma = char ','
