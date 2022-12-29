module Day2 where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines)
import Effect (Effect)
import Data.Char (toCharCode)
import Data.String.CodeUnits (charAt)
import Effect.Unsafe (unsafePerformEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Data.Ord (abs)
import Lib (Part(..), getInput)

rowScore :: Maybe Int -> String -> Maybe Int
rowScore sum row =
  do
    prev <- sum
    fst <- charAt 0 row
    snd <- charAt 2 row
    let opponent = toCharCode fst - 64
    let myChoice = toCharCode snd - 87
    let score = prev + myChoice + 3 * (abs (mod (myChoice - opponent + 1) 3))
    pure score

sndScore :: Maybe Int -> String -> Maybe Int
sndScore sum row =
  do
    prev <- sum
    fst <- charAt 0 row
    snd <- charAt 2 row
    let opponent = toCharCode fst - 65
    let res = (toCharCode snd) - 88
    let myChoice = mod (opponent - 1 + res) 3
    let score = prev + myChoice + 1 + 3 * res
    pure score

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day2.txt"
        case part of
          First -> pure (Array.foldl rowScore (Just 0) inputs)
          Second -> pure (Array.foldl sndScore (Just 0) inputs)
  in
    unsafePerformEffect doPart
      # show