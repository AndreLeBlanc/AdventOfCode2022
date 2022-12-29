module Day4 where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Effect.Unsafe (unsafePerformEffect)
import Lib (foldSum, getInput, Part(..))

rowScore :: Part -> String -> Int
rowScore part row =
  let
    score :: Array (Maybe Int) -> Maybe Int
    score endPoints =
      do
        case endPoints of
          [ firstStart, firstEnd, secondStart, secondEnd ] ->
            pure
              if
                case part of
                  First ->
                    (firstStart <= secondStart && secondEnd <= firstEnd)
                      || (firstStart >= secondStart && secondEnd >= firstEnd)
                  Second ->
                    (firstStart <= secondEnd && secondStart <= firstEnd) then 1
              else 0
          _ -> Nothing
  in
    row
      # String.split (Pattern ",")
      # Array.concatMap (String.split (Pattern "-"))
      <#> Int.fromString
      # score
      # fromMaybe 0

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day4.txt"
        pure (foldSum (rowScore part) inputs)
  in
    unsafePerformEffect doPart
      # show