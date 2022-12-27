module Day5 where

import Prelude

import Data.Array (drop, foldr, uncons, reverse, dropEnd, filter, head, index, mapMaybe, snoc, span, splitAt, take, updateAt)
import Data.Array.NonEmpty as NEA
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex (match)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.String.Utils (lines, toCharArray)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Operation = { num :: Int, source :: Int, destination :: Int }

type Rearrangement = { stacks :: Array (Array String), moves :: Array Operation }

transpose :: forall a. Array (Array a) -> Array (Array a)
transpose = go mempty
  where
  go :: Array (Array a) -> Array (Array a) -> Array (Array a)
  go acc xs =
    case traverse head xs of
      Nothing -> acc
      Just heads -> go (snoc acc heads) (map (drop 1) xs)

parseStacks :: String -> Array String
parseStacks = go mempty <<< toCharArray
  where
  go :: Array String -> Array String -> Array String
  go acc st =
    case take 3 st of
      [ _, ch, _ ] -> go (snoc acc ch) (drop 4 st)
      _ -> acc

parseMoves :: String -> Maybe Operation
parseMoves st =
  do
    digits <-
      mapMaybe (flip bind Int.fromString)
        <<< NEA.toArray
        <$> match (unsafeRegex "\\d+" global) st
    case digits of
      [ num, source, destination ] -> pure { num: num, source: source - 1, destination: destination - 1 }
      _ -> Nothing

parseInput :: String -> Effect Rearrangement
parseInput path =
  do
    dat <- lines <$> readTextFile UTF8 path
    let _raw@{ init, rest } = span (not eq "") dat
    { stacks:
        map
          ( filter (not eq " ")
              <<< dropEnd 1
          )
          $ transpose
          $ map parseStacks init
    , moves: mapMaybe parseMoves rest
    }
      # pure

solve :: Rearrangement -> Boolean -> String
solve job part2 =
  let
    reversePart2 :: Array String -> Array String
    reversePart2 arr = if part2 then arr else reverse arr
  in
    case job.moves of
      [] -> foldr (\stack acc -> (fromMaybe "" (head stack)) <> acc) "" job.stacks
      moves ->
        do
          { head: command, tail: commandTail } <- uncons moves
          donor <- index job.stacks command.source
          reciever <- index job.stacks command.destination
          let giver = splitAt command.num donor
          updatedGiver <- updateAt command.source giver.after job.stacks
          updatedLayout <- updateAt command.destination ((reversePart2 giver.before) <> reciever) updatedGiver
          solve { moves: commandTail, stacks: updatedLayout } part2 # pure
          # fromMaybe "failed"

-- Part 1
part1 :: String
part1 =
  let
    readP1 =
      do
        inputs <- parseInput "inputs/day5.txt"
        solve inputs false # pure

  in
    unsafePerformEffect readP1
      # show

part2 :: String
part2 =
  let
    readP2 =
      do
        inputs <- parseInput "inputs/day5.txt"
        solve inputs true # pure

  in
    unsafePerformEffect readP2
      # show
