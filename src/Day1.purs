module Day1 where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

sumRows :: { sums :: Array Int, ints :: Int } -> String -> { sums :: Array Int, ints :: Int }
sumRows { sums, ints } row =
  case row of
    "" -> { sums: (Array.cons ints sums), ints: 0 }
    str -> { sums, ints: ints + (Int.fromString str # fromMaybe 0) }

getInput :: Effect (Array Int)
getInput =
  do
    raw <- readTextFile UTF8 "inputs/day1.txt"
    let inputRows = lines raw
    let sums = Array.foldl sumRows { sums: [], ints: 0 } inputRows
    pure sums.sums

part1 :: String
part1 =
  let
    readP1 =
      do
        inputs <- getInput
        let partOne = (Array.foldl max 0 inputs)
        pure partOne
  in
    unsafePerformEffect readP1
      # show

part2 :: String
part2 =
  let
    readP2 =
      do
        inputs <- getInput
        let partOne = Array.sort inputs
        pure partOne
  in
    unsafePerformEffect readP2
      # Array.reverse
      # Array.take 3
      # Array.foldl (+) 0
      # show