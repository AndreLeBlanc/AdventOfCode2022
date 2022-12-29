module Day9 where

import Prelude

import Data.Array (foldl, head, replicate, reverse, snoc, drop)
import Data.Int (fromString, toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (abs)
import Data.Set (fromFoldable, size)
import Data.String (splitAt)
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInput, dec, Part(..))

type Coordinate = { x :: Int, y :: Int }

type Rope = { head :: Coordinate, tail :: Array Coordinate, been :: Array (Array Coordinate) }

dist :: Int -> Int -> Int
dist head tail | tail < head = tail + 1
dist head tail | tail > head = tail - 1
dist _ tail = tail

move :: Rope -> Coordinate -> Rope
move rope { x: x, y: y } =
  let
    step 0 = 0
    step d = if d > 0 then 1 else -1
    newHead = { x: rope.head.x + (step x), y: rope.head.y + (step y) }
    newTail prev curr = { x: dist prev.x curr.x, y: dist prev.y curr.y }
    last prev = head (reverse prev) # fromMaybe { x: 0, y: 0 }

    tails :: Array Coordinate -> Coordinate -> Array Coordinate
    tails prev curr =
      case abs (toNumber ((last prev).x - curr.x)) > 1.0 || abs (toNumber ((last prev).y - curr.y)) > 1.0 of
        true -> snoc prev (newTail (last prev) curr)
        false -> snoc prev curr

    updateTails = foldl tails [ newHead ] rope.tail
      # drop 1
  in
    case 1.0 <= (abs (toNumber x)) || 1.0 <= (abs (toNumber y)) of
      true -> move { head: newHead, tail: updateTails, been: snoc rope.been updateTails   } { x: dec x, y: dec y }
      _ -> rope

performMove :: Rope -> String -> Rope
performMove rope mov =
  let
    toInt str = fromString str # fromMaybe 0
  in
    case splitAt 2 mov of
      { before: "U ", after } -> move rope { x: 0, y: (toInt after) }
      { before: "L ", after } -> move rope { x: -(toInt after), y: 0 }
      { before: "D ", after } -> move rope { x: 0, y: -(toInt after) }
      { before: _, after } -> move rope { x: (toInt after), y: 0 }

sizer :: Rope -> Int
sizer r =
  map (\x -> head (reverse x) # fromMaybe { x: 0, y: 0 }) r.been 
    # fromFoldable
    # size

moves :: Array String -> Int -> Int
moves mov size =
  foldl performMove { head: { x: 0, y: 0 }, tail: replicate size { x: 0, y: 0 }, been: [[ { x: 0, y: 0 } ]] } mov
    # sizer

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day9.txt"
        case part of
         First -> moves inputs 1 # pure
         Second -> moves inputs 9 # pure

  in
    unsafePerformEffect doPart
      # show