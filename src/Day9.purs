module Day9 where

import Prelude

import Data.Array (foldl, snoc)
import Data.Int (fromString, toNumber)
import Data.Maybe (fromMaybe)
import Data.Number (abs)
import Data.Set (fromFoldable, size)
import Data.String (splitAt)
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInput, dec)

type Coordinate = { x :: Int, y :: Int }

type Rope = { head :: Coordinate, tail :: Coordinate, been :: Array Coordinate }

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
    newTail = { x: dist newHead.x rope.tail.x, y: dist newHead.y rope.tail.y }
  in
    case 1.0 <= (abs (toNumber x)) || 1.0 <= (abs (toNumber y)) of
      true ->
        case abs (toNumber (newHead.x - rope.tail.x)) > 1.0 || abs (toNumber (newHead.y - rope.tail.y)) > 1.0 of
          true -> move { head: newHead, tail: newTail, been: snoc rope.been newTail } { x: dec x, y: dec y }
          false -> move { head: newHead, tail: rope.tail, been: rope.been } { x: dec x, y: dec y }
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
  fromFoldable r.been
    # size

moves :: Array String -> Int
moves mov =
  foldl performMove { head: { x: 0, y: 0 }, tail: { x: 0, y: 0 }, been: [ { x: 0, y: 0 } ] } mov
    # sizer

part1 :: String
part1 =
  let
    readP1 =
      do
        inputs <- getInput "inputs/day9.txt"
        moves inputs # pure

  in
    unsafePerformEffect readP1
      # show

part2 = "todo"