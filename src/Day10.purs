module Day10 where

import Prelude

import Data.Array (foldl, snoc, index)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String as S
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInput, Part(..))

type Signal = { register :: Int, strength :: Int, res :: Array Int, print :: Array Int }

execute :: Signal -> String -> Signal
execute signal op =
  let
    added :: Int
    added = fromString (S.drop 5 op) # fromMaybe 0 # add signal.register

    addRes :: Array Int
    addRes =
      signal.res <> [ (signal.strength * signal.register), ((signal.strength + 1) * signal.register) ]

    addPrint :: Array Int
    addPrint =
      signal.print <> [ (signal.register), (signal.register) ]
  in
    case S.take 4 op of
      "noop" -> signal { strength = signal.strength + 1, res = snoc signal.res (signal.strength * signal.register), print = snoc signal.print signal.register }
      _ -> signal { register = added, strength = signal.strength + 2, res = addRes, print = addPrint }

moves :: Array String -> Signal
moves program =
  foldl execute { register: 1, strength: 1, res: [ 0 ], print: [ 0 ] } program

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day10.txt"
        case part of
          First -> foldl (\acc x -> acc + (index (moves inputs).res x # fromMaybe 0)) 0 [ 20, 60, 100, 140, 180, 220 ] # pure
          Second -> foldl (\acc x -> acc + (index (moves inputs).res x # fromMaybe 0)) 0 [ 20, 60, 100, 140, 180, 220 ] # pure --(foldl execute { register: 1, strength: 1, res: [ 0 ], print: [ 0 ] } inputs).print # pure

  in
    unsafePerformEffect doPart
      # show