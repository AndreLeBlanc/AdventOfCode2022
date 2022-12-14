module Main where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Day1 as Day1
import Day2 as Day2
import Day3 as Day3
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)

type Year = Int
type Day = Int
data Part = NoPart | First | Second

data Command = Advent Year Day Part

toPart :: Maybe Int -> Part
toPart partNum =
  case partNum of
    Just 1 -> First
    Just 2 -> Second
    _ -> NoPart

parseArgs :: Array String -> Maybe Command
parseArgs args =
  do
    year <- Int.fromString =<< Array.head args
    day <- Int.fromString =<< Array.index args 1
    let part = (Int.fromString =<< Array.index args 2) # toPart
    Advent year day part # pure

runCommand :: Array String -> String
runCommand args =
  do
    let commandArgs = Array.drop 2 args
    command <- parseArgs commandArgs
    pure case command of
      (Advent 2022 1 First) -> Day1.part1
      (Advent 2022 1 Second) -> Day1.part2
      (Advent 2022 1 NoPart) -> "part one: " <> Day1.part1 <> ", part two: " <> Day1.part2
      (Advent 2022 2 First) -> Day2.part1
      (Advent 2022 2 Second) -> Day2.part2
      (Advent 2022 2 NoPart) -> "part one: " <> Day2.part1 <> ", part two: " <> Day2.part2
      (Advent 2022 3 First) -> Day3.part1
      (Advent 2022 3 Second) -> Day3.part2
      (Advent 2022 3 NoPart) -> "part one: " <> Day3.part1 <> ", part two: " <> Day3.part2
      _ -> "Didn't find instance of command"
    # fromMaybe "couldn't parse command"

main :: Effect Unit
main = do
  args <- argv
  log $ show (runCommand args)