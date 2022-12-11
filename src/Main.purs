module Main where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Int as Int
import Effect.Console (log)
import Node.Process (argv)
import Day1 as Day1

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
      (Advent 2022 10 First) -> Day1.part1
      (Advent 2022 10 Second) -> Day1.part2
      (Advent 2022 10 NoPart) -> "nada"
      _ -> "Didn't find instance of command"
    # fromMaybe "couldn't parse command"

main :: Effect Unit
main = do
  args <- argv
  log $ show (runCommand args)