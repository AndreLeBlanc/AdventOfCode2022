module Day6 where

import Prelude

import Data.Array (elem, foldl, length)
import Data.Int (toStringAs, decimal)
import Data.Set (fromFoldable, size)
import Data.String (take, drop)
import Data.String.CodeUnits (toCharArray)
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInputStr)

findPaketStart :: Int -> String -> String
findPaketStart pos signal =
  case toCharArray (take 4 signal) of
    [ i, j, k, l ] ->
      if elem i [ j, k, l ] || elem j [ k, l ] || k == l then findPaketStart (pos + 1) (drop 1 signal)
      else toStringAs decimal (pos + 1)
    _ -> "fail"

findMessageStart :: Int -> String -> String
findMessageStart pos signal =
  let
    potentialMessageStart = toCharArray (take 14 signal)

    isMessageStart :: Array Char -> Boolean
    isMessageStart potMess =
      fromFoldable potMess # size # eq 14
  in
    case isMessageStart potentialMessageStart of
      true -> toStringAs decimal (pos + 1)
      false -> findMessageStart (pos + 1) (drop 1 signal)

part1 :: String
part1 =
  let
    readP1 =
      do
        inputs <- getInputStr "inputs/day6.txt"
        findPaketStart 3 inputs # pure

  in
    unsafePerformEffect readP1
      # show

part2 :: String
part2 =
  let
    readP2 =
      do
        inputs <- getInputStr "inputs/day6.txt"
        findMessageStart 13 inputs # pure

  in
    unsafePerformEffect readP2
      # show
