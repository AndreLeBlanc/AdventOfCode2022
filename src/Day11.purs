module Day11 where

import Prelude

import Data.Array (foldl, filter, length, mapMaybe, partition, snoc, sort, take, reverse)
import Data.Array.NonEmpty as NEA
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (fromString, toNumber)
import Data.Map (fromFoldable, lookup, Map, update)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as N
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInput, Part(..), group)

data Op = Add Number | Mul Number | Square
type Monkey = { id :: Int, starting :: Array Number, op :: Op, divisible :: Number, throw :: Int, other :: Int, tot :: Number }
type Starts = Map Int (Array Monkey)

recieving ∷ Starts
recieving = fromFoldable
  [ Tuple 0 []
  , Tuple 1 []
  , Tuple 2 []
  , Tuple 3 []
  , Tuple 4 []
  , Tuple 5 []
  , Tuple 6 []
  , Tuple 7 []
  ]

operPattern = unsafeRegex "(\\+|\\*|\\d+)" global :: Regex
digitPattern = unsafeRegex "\\d+" global :: Regex

lastNum :: String -> Maybe Int
lastNum = flip bind (fromString <=< NEA.head) <<< match digitPattern

matchNum :: String -> Maybe Number
matchNum = flip bind (N.fromString <=< NEA.head) <<< match digitPattern

matchItemData :: String -> Maybe (Array Number)
matchItemData raw =
  let
    ints = fromMaybe mempty
      $ NEA.mapMaybe (flip bind N.fromString) <$> match digitPattern raw
  in
    pure
      $ mapWithIndex
          ( \_ worry -> worry
          )
          ints

matchOperation :: String -> Maybe Op
matchOperation st =
  case match operPattern st of
    Nothing -> pure Square
    Just nea ->
      case NEA.mapMaybe identity nea of
        [ "+", n ] -> Add <$> N.fromString n
        [ "*", n ] -> Mul <$> N.fromString n
        _ -> pure Square

toMonkey :: Array String -> Maybe Monkey
toMonkey rows =
  do
    case rows of
      [ name, start, op, test, t, f, _ ] ->
        do

          { id: _, starting: _, op: _, divisible: _, throw: _, other: _, tot: 0.0 }
            <$> lastNum name
            <*> matchItemData start
            <*> matchOperation op
            <*> matchNum test
            <*> lastNum t
            <*> lastNum f
      _ -> Nothing

fillNext :: Starts -> Monkey -> Starts
fillNext startz monkey =
  let
    newWorry :: Number -> Number
    newWorry worry =
      case monkey.op of
        Add a -> worry + a / 3.0
        Mul a -> worry * a / 3.0
        Square -> worry * worry / 3.0

    getReciever reciever start = lookup reciever start # fromMaybe []

    toNext :: Starts -> Number -> Starts
    toNext start item =
      case (mod (newWorry item) monkey.divisible) == 0.0 of
        true -> update (\_ -> snoc (getReciever monkey.throw start) (newWorry item) # pure) monkey.throw start
        false -> update (\_ -> snoc (getReciever monkey.throw start) (newWorry item) # pure) monkey.other start
  in
    foldl toNext startz monkey.starting

round :: Starts -> Int -> Array Monkey -> Array Number
round play rounds monkies =
  let
    findStarts id mp =
      lookup id mp # fromMaybe []

    updateMonkies :: Array Monkey -> Starts -> Array Monkey
    updateMonkies monk ply =
      foldl (\acc mon -> acc <> [ mon { tot = mon.tot + (length mon.starting # toNumber), starting = findStarts mon.id ply } ]) [] monk
  in
    case rounds of
      0 -> map _.tot monkies -- # sort # reverse # take 2 # foldl (*) 1.0
      _ ->
        updateMonkies monkies play
          # round (foldl fillNext recieving monkies) (rounds - 1)

parseMonkies :: Int -> Array (Array String) -> Array Number
parseMonkies rounds input =
  let
    monkies = mapMaybe toMonkey input
  in
    round (foldl fillNext recieving monkies) rounds monkies

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day11.txt"
        let grouped = group 7 (inputs <> [ " " ])
        case part of
          First -> parseMonkies 1 grouped
            # show
            # pure
          Second -> parseMonkies 20 grouped
            # show
            # pure

  in
    unsafePerformEffect doPart
      # show