module Lib where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.String.Common (trim)
import Data.String.Utils (lines)

foldSum :: forall a. (a -> Int) -> Array a -> Int
foldSum fun inputs = Array.foldl (\acc x -> acc + fun x) 0 inputs

getInput :: String -> Effect (Array String)
getInput path = (readTextFile UTF8 >=> trim >>> lines >>> pure) path