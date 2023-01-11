module Day7 where

import Prelude

import Data.Array (cons, uncons, drop)
import Data.Foldable (foldl, sum)
import Data.Generic.Rep (class Generic)
import Data.Map (Map, empty, insertWith, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number (fromString)
import Data.Show.Generic (genericShow)
import Data.String.Common (joinWith)
import Data.String.Utils (words)
import Effect.Unsafe (unsafePerformEffect)
import Lib (getInput, Part(..), tails)

data InDir
  = Root
  | NoOp
  | SwitchDir String
  | UpLevel
  | FileInfo Number String

derive instance Generic InDir _

instance Show InDir where
  show = genericShow

type Hist = Array InDir

type File = { file :: String, size :: Number }

type Sys =
  { files :: Map String (Array File)
  , namedDir :: String
  , tilda :: Array String
  }

emptySys =
  { files: empty
  , tilda: mempty
  , namedDir: "/"
  } :: Sys

nav :: Hist -> Sys -> Sys
nav hist sys =
  do
    { head, tail } <- uncons hist

    let
      a = case head of
        Root ->
          sys
            { tilda = pure "/"
            , namedDir = "/"
            }

        UpLevel ->
          let
            newPath = drop 1 sys.tilda
          in
            sys
              { tilda = newPath
              , namedDir = joinWith "|" newPath
              }

        SwitchDir dir ->
          let
            newPath = cons dir sys.tilda
          in
            sys
              { tilda = newPath
              , namedDir = joinWith "|" newPath
              }

        FileInfo mem fname ->
          let
            handle = { file: fname, size: mem }
          in
            sys
              { files = foldl
                  ( \fs cwp ->
                      insertWith append (joinWith "|" cwp) (pure handle) fs
                  )
                  sys.files
                  (tails sys.tilda)
              }

        NoOp ->
          sys
    nav tail a # pure
    # fromMaybe sys

sumFiles paths = map _.size paths # sum

maxTotFileSize :: (Number -> Number -> Boolean) -> Number -> Sys -> Number
maxTotFileSize cmp limit { files } =
  foldl
    ( \sm paths ->
        if cmp (sumFiles paths) limit then sm + (sumFiles paths) else sm
    )
    0.0
    files

findMinUpdSector :: Sys -> Number
findMinUpdSector sys =
  let
    remaining :: Sys -> Number
    remaining system =
      do
        r <- lookup "/" system.files :: Maybe (Array File)
        let sizes = map _.size r :: Array Number
        sum sizes # pure
        # fromMaybe 0.0

    minSpace = remaining sys - 40000000.0
  in
    foldl
      ( \sector paths ->
          if (sumFiles paths) > minSpace && (sumFiles paths) < sector then (sumFiles paths)
          else sector
      )
      30000000.0
      sys.files

toCommand :: Array String -> InDir
toCommand [ fst, snd ] = FileInfo (fromString fst # fromMaybe 0.0) snd

toCommand [ _, _, path ] =
  case path of
    ".." -> UpLevel
    "/" -> Root
    _ -> SwitchDir path

toCommand _ = NoOp

solve :: Part -> String
solve part =
  let
    doPart =
      do
        inputs <- getInput "inputs/day7.txt"
        let parsed = map (toCommand <<< words) inputs
        case part of
          First -> nav parsed emptySys # maxTotFileSize (<) 100000.0 # pure
          Second -> nav parsed emptySys # findMinUpdSector # pure
  in
    unsafePerformEffect doPart
      # show
