module Day7 where

import Prelude

import Data.Array (head)
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInputStr)

newtype Directory = Directory { size :: Int, subDirs :: Array Directory }

-- parseCommands :: Array String -> Directory
-- parseCommands input =
--     case head input of
-- 
--     _ -> 

part1 :: String
part1 = "todo"
--  let
--    readP1 =
--      do
--        inputs <- getInputStr "inputs/day7.txt"
--        parseCommands inputs # pure
--
--  in
--    unsafePerformEffect readP1
--      # show
