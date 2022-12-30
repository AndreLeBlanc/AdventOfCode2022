module Day10
  ( Signal
  , display
  , execute
  , moves
  , printer
  , solve
  ) where

import Prelude

import Data.Array (concat, foldl, index, replicate, snoc, updateAt, zip, (..))
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInput, Part(..))

type Signal = { register :: Int, res :: Array Int }

execute :: Signal -> String -> Signal
execute signal op =
  let
    added :: Int
    added = fromString (S.drop 5 op) 
      # fromMaybe 0 
      # add signal.register
  in
    case S.take 4 op of
      "noop" -> signal { res = snoc signal.res signal.register }
      _ -> signal { register = added, res = signal.res <> [ signal.register, signal.register ] }

moves :: Array String -> Signal
moves program =
  foldl execute { register: 1, res: [] } program

display :: Array String
display = replicate 240 " "

pixel :: Array String -> Tuple Int Int -> Array String
pixel disp (Tuple pxl register) =
  case (register +1 == mod pxl 40 || register == mod pxl 40 || register - 1 == mod pxl 40) of
    true -> updateAt pxl "#" disp # fromMaybe disp
    _ -> disp

printer :: Array String -> String
printer inputs =
  (moves inputs).res
    # zip (0 .. 239)
    # foldl pixel display
    # foldl (<>) ""

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day10.txt"
        case part of
          First -> foldl (\acc x -> acc + (x * (index (moves inputs).res (x-1) # fromMaybe 0))) 0 [ 20, 60, 100, 140, 180, 220 ] 
            # show 
            # pure
          Second -> printer inputs # pure

  in
    unsafePerformEffect doPart
      # show