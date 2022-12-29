module Day10 where

import Prelude

import Data.Array (foldl, head, replicate, reverse, snoc, drop)
import Data.Int (fromString, toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (abs)
import Data.Set (fromFoldable, size)
import Data.String (splitAt)
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInput, dec, Part(..))

moves x = "todo"

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day9.txt"
        case part of
          First -> moves inputs # pure
          Second -> moves inputs # pure

  in
    unsafePerformEffect doPart
      # show