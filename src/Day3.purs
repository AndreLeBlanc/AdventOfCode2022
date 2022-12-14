module Day3 where

import Prelude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.Maybe (Maybe, fromMaybe)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Common (trim)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

getInput :: Effect (Array String)
getInput = (readTextFile UTF8 >=> trim >>> lines >>> pure) "inputs/day3.txt"

prioritityVal :: Char -> Int
prioritityVal charVal =
  case 96 < toCharCode charVal of
    true -> toCharCode charVal - 96
    _ -> toCharCode charVal - 38

rowScore :: Int -> String -> Int
rowScore sum row =
  let
    inBothCompartments :: Array Char
    inBothCompartments =
      toCharArray row
        # Array.splitAt ((String.length row) / 2)
        # (\splits -> Array.intersect splits.after splits.before)
    score =
      do
        inBoth <- Array.head inBothCompartments
        let res = sum + prioritityVal inBoth
        pure res
  in
    score # fromMaybe 0

part1 :: String
part1 =
  let
    readP1 =
      do
        inputs <- getInput
        pure (Array.foldl rowScore 0 inputs)

  in
    unsafePerformEffect readP1
      # show

triplets :: Array String -> Int -> Int
triplets rows sum =
  let
    three :: { before :: Array String, after :: Array String }
    three = Array.splitAt 3 rows

    tripletScore :: Array (Array Char) -> Int
    tripletScore chars =
      let
        calcScore =
          do
            { head, tail } <- Array.uncons chars
            let intersects = Array.foldl Array.intersect head tail
            common <- Array.head intersects
            pure (prioritityVal common)
      in
        calcScore
          # fromMaybe 0
  in
    case Array.length three.before of
      3 ->
        map toCharArray three.before
          # tripletScore
          # add sum
          # triplets three.after
      _ -> sum

part2 :: String
part2 =
  let
    readP2 =
      do
        inputs <- getInput
        pure (triplets inputs 0)
  in
    unsafePerformEffect readP2
      # show

