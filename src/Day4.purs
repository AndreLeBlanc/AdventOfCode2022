module Day4 where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe, fromMaybe)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect.Unsafe (unsafePerformEffect)
import Lib (foldSum, getInput)

data Part = Part1 | Part2

derive instance Eq Part

rowScore :: Part -> String -> Int
rowScore part row =
  let
    score :: Array (Maybe Int) -> Maybe Int
    score endPoints =
      do
        firstStart <- Array.head endPoints
        firstEnd <- Array.index endPoints 1
        secondStart <- Array.index endPoints 2
        secondEnd <- Array.index endPoints 3
        let
          res = case part of
            Part1 ->
              (firstStart <= secondStart && secondEnd <= firstEnd)
                || (firstStart >= secondStart && secondEnd >= firstEnd)
            Part2 ->
              (firstStart <= secondEnd && secondStart <= firstEnd)
        pure if res then 1 else 0
  in
    row
      # String.split (Pattern ",")
      # Array.concatMap (String.split (Pattern "-"))
      <#> Int.fromString
      # score
      # fromMaybe 0

part1 :: String
part1 =
  let
    readP1 =
      do
        inputs <- getInput "inputs/day4.txt"
        pure (foldSum (rowScore Part1) inputs)

  in
    unsafePerformEffect readP1
      # show

part2 :: String
part2 =
  let
    readP2 =
      do
        inputs <- getInput "inputs/day4.txt"
        pure (foldSum (rowScore Part2) inputs)

  in
    unsafePerformEffect readP2
      # show
