module Main where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Day1 as Day1
import Day2 as Day2
import Day3 as Day3
import Day4 as Day4
import Day5 as Day5
import Day6 as Day6
import Day7 as Day7
import Day8 as Day8
import Day9 as Day9
import Day10 as Day10
import Effect (Effect)
import Effect.Console (log)
import Node.Process (argv)
import Lib (Part(..), Parts(..))
import Data.Map (fromFoldable, lookup, Map)
import Data.Tuple (Tuple(..))

type Year = Int
type Day = Int

data Command = Advent Year Day Parts

days ∷ Map Int (Part → String)
days = fromFoldable
  [ Tuple 1 Day1.solve
  , Tuple 2 Day2.solve
  , Tuple 3 Day3.solve
  , Tuple 4 Day4.solve
  , Tuple 5 Day5.solve
  , Tuple 6 Day6.solve
  , Tuple 7 Day7.solve
  , Tuple 8 Day8.solve
  , Tuple 9 Day9.solve
  , Tuple 10 Day10.solve
  ]

toPart :: Maybe Int -> Parts
toPart partNum =
  case partNum of
    Just 1 -> Single First
    Just 2 -> Single Second
    _ -> NoPart

parseArgs :: Array String -> Maybe Command
parseArgs args =
  do
    year <- Int.fromString =<< Array.head args
    day <- Int.fromString =<< Array.index args 1
    let part = (Int.fromString =<< Array.index args 2) # toPart
    Advent year day part # pure

runDay :: Int -> Part -> String
runDay day part =
  case lookup day days of
    Nothing -> "day not implemented"
    Just solver -> solver part

runCommand :: Array String -> String
runCommand args =
  do
    let commandArgs = Array.drop 2 args
    command <- parseArgs commandArgs
    pure case command of
      (Advent 2022 day (Single part)) -> runDay day part
      (Advent 2022 day NoPart) -> "part one: " <> runDay day First <> ", part two: " <> runDay day Second
      _ -> "Didn't find instance of command"
    # fromMaybe "couldn't parse command"

main :: Effect Unit
main = do
  args <- argv
  log (runCommand args)