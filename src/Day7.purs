module Day7 where

import Prelude

import Data.Array (head)
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInputStr, Part(..))

newtype Directory = Directory { size :: Int, subDirs :: Array Directory }

-- parseCommands :: Array String -> Directory
-- parseCommands input =
--     case head input of
-- 
--     _ -> 
solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInputStr "inputs/day6.txt"
        case part of
          First -> "todo" # pure
          Second -> "todo" # pure
  in
    unsafePerformEffect doPart
      # show
