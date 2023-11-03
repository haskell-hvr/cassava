module Generic.Prefix where

import qualified Data.List as List
import           Data.Maybe


dropPrefix :: String -> String -> String
dropPrefix pfx = fromMaybe (error "invalid prefix") . List.stripPrefix pfx
