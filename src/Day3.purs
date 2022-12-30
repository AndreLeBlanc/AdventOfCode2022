module Day3 where

import Prelude

import Data.Array as Array
import Data.Char (toCharCode)
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Effect.Unsafe (unsafePerformEffect)
import Lib (Part(..), getInput)

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

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day3.txt"
        case part of
          First -> pure (Array.foldl rowScore 0 inputs)
          Second -> pure (triplets inputs 0)
  in
    unsafePerformEffect doPart
      # show