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

getInput :: Effect (Array String)
getInput =
  do
    raw <- readTextFile UTF8 "inputs/day2.txt"
    lines raw # pure

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

part1 :: String
part1 =
  let
    readP1 =
      do
        inputs <- getInput
        pure (Array.foldl rowScore (Just 0) inputs)

  in
    unsafePerformEffect readP1
      # show

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

part2 :: String
part2 =
  let
    readP2 =
      do
        inputs <- getInput
        pure (Array.foldl sndScore (Just 0) inputs)

  in
    unsafePerformEffect readP2
      # show
