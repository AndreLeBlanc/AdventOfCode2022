module Lib where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (trim)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Part = First | Second

derive instance Eq Part

data Parts = NoPart | Single Part

foldSum :: forall a. (a -> Int) -> Array a -> Int
foldSum fun inputs = Array.foldl (\acc x -> acc + fun x) 0 inputs

getInput :: String -> Effect (Array String)
getInput path = (readTextFile UTF8 >=> trim >>> lines >>> pure) path

getInputStr :: String -> Effect String
getInputStr path = (readTextFile UTF8 >=> trim >>> pure) path

group :: forall a. Int -> Array a -> Array (Array a)
group _ [] = []
group n l
  | n > 0 =  [(Array.take n l)] <> ((group n (Array.drop n l))) 
  | otherwise = []

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose = go mempty
  where
  go :: Array (Array a) -> Array (Array a) -> Array (Array a)
  go acc xs =
    case traverse Array.head xs of
      Nothing -> acc
      Just heads -> go (Array.snoc acc heads) (map (Array.drop 1) xs)

subLen :: forall a. (a -> Boolean) -> Array a -> Int
subLen fun xs = min (1 + Array.length (Array.takeWhile fun xs)) (Array.length xs)

dec :: Int -> Int
dec x | x > 0 = x - 1
dec x | x < 0 = x + 1
dec x = x