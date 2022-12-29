module Day1 where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Effect.Unsafe (unsafePerformEffect)
import Lib (Part(..), getInput)

type CarriedCals = { sums :: Array Int, ints :: Int }

sumRows :: CarriedCals -> String -> CarriedCals
sumRows { sums, ints } row =
  case row of
    "" -> { sums: (Array.cons ints sums), ints: 0 }
    str -> { sums, ints: ints + (Int.fromString str # fromMaybe 0) }

calories :: Array String -> Array Int
calories inputRows =
  (Array.foldl sumRows { sums: [], ints: 0 } inputRows).sums

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day1.txt"
        case part of
          First -> calories inputs
            # Array.foldl max 0
            # pure
          Second ->
            calories inputs
              # Array.sort
              # Array.reverse
              # Array.take 3
              # Array.foldl (+) 0
              # pure
  in
    unsafePerformEffect doPart
      # show