module Day8 where

import Prelude

import Data.Array (drop, foldl, index, length, reverse, splitAt, zipWith, (..), concat, takeWhile)
import Data.Char (toCharCode)
import Data.Maybe (fromMaybe)
import Data.Maybe (Maybe(..))
import Data.Foldable (maximum)
import Data.String.CodeUnits (toCharArray)
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInput, subLen, transpose, Part(..))

visibleRow :: Array Int -> Array Boolean
visibleRow row =
  let
    visibleTree :: Int -> Boolean
    visibleTree tree =
      let
        { before, after } = splitAt tree row
        treeHeight = index row tree # fromMaybe 0
      in
        (foldl (\res next -> res && next < treeHeight) true before)
          || (foldl (\res next -> res && next < treeHeight) true (drop 1 after))

  in
    map visibleTree (0 .. (length row))

heights :: Array String -> Array (Array Int)
heights forrest =
  map toCharArray forrest
    <#> map toCharCode

countTrees :: Array String -> Int
countTrees forrest =
  let
    rows :: Array (Array Boolean)
    rows = map visibleRow (heights forrest)

    cols :: Array (Array Boolean)
    cols = transpose (map visibleRow (transpose (heights forrest)))
  in
    zipWith (\a b -> zipWith (||) a b) rows cols
      # concat
      # foldl (\acc tree -> if tree then acc + 1 else acc) 0

part1 :: String
part1 =
  let
    readP1 =
      do
        inputs <- getInput "inputs/day8.txt"
        countTrees inputs # pure
  in
    unsafePerformEffect readP1
      # show

sightLineRow :: Array Int -> Array Int
sightLineRow row =
  let
    visibleTree :: Int -> Int
    visibleTree tree =
      let
        { before, after } = splitAt tree row
        treeHeight = index row tree # fromMaybe 0
      in
        (subLen (\a -> a < treeHeight) (reverse before)) * (subLen (\a -> a < treeHeight) (drop 1 after))
  in
    map visibleTree (0 .. (length row))

countSightline :: Array String -> Maybe Int
countSightline forrest =
  let
    rows :: Array (Array Int)
    rows = map sightLineRow (heights forrest)

    cols :: Array (Array Int)
    cols = transpose (map sightLineRow (transpose (heights forrest)))
  in
    zipWith (\a b -> zipWith (*) a b) rows cols
      # concat
      # maximum

part2 :: String
part2 =
  let
    readP2 =
      do
        inputs <- getInput "inputs/day8.txt"
        countSightline inputs # pure

  in
    unsafePerformEffect readP2
      # show

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day8.txt"
        case part of
          First -> countTrees inputs # pure
          Second -> countSightline inputs # fromMaybe 0 # pure
  in
    unsafePerformEffect doPart
      # show
