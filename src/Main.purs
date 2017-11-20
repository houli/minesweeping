module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foldable (sum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Matrix (Matrix, empty, fromArray, get, indexedMap)

data Mine = Boom
          | Safe

derive instance genericMine :: Generic Mine _
instance showMine :: Show Mine where
  show = genericShow

data Result = Mine
            | Count Int

instance showResultBit :: Show Result where
  show Mine = "*"
  show (Count x) = show x

type ResultField = Matrix Result

testMatrix :: Matrix Mine
testMatrix = fromMaybe empty $ fromArray [[Boom, Safe, Safe, Safe], [Safe, Safe, Safe, Safe], [Safe, Boom, Safe, Safe], [Safe, Safe, Safe, Safe]]

theOtherMatrix :: Matrix Mine
theOtherMatrix = fromMaybe empty $ fromArray [[Boom, Boom, Safe, Safe, Safe], [Safe, Safe, Safe, Safe, Safe], [Safe, Boom, Safe, Safe, Safe]]

collectSums :: Matrix Mine -> Int -> Int -> Mine -> Result
collectSums matrix x y mine = case mine of
  Boom -> Mine
  Safe -> Count <<< sum <<< map detectMine $ adjacentPositions x y
  where
    detectMine :: Tuple Int Int -> Int
    detectMine (Tuple x y) = case get x y matrix of
      Just Boom -> 1
      _ -> 0

adjacentPositions :: Int -> Int -> List (Tuple Int Int)
adjacentPositions x y = do
  xx <- (x + 1) : (x - 1) : x : Nil
  yy <- (y + 1) : (y - 1) : y : Nil
  pure $ Tuple xx yy

solve :: Matrix Mine -> Matrix Result
solve matrix = indexedMap (collectSums matrix) matrix

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Field #1:"
  log <<< show <<< solve $ testMatrix
  log ""
  log "Field #2:"
  log <<< show <<< solve $ theOtherMatrix
