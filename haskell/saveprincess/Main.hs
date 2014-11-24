-- saveprincess challenge https://www.hackerrank.com/challenges/saveprincess
module Main where

import Data.List
import Data.Maybe
import Debug.Trace

type Position = (Int, Int)
type Matrix = [(Position, Char)]
type Finder = Char -> Bool
type Path = [Direction]

data Direction = MoveLeft | MoveRight | MoveUp | MoveDown
  deriving (Show)

main :: IO ()
main = do
  allInput <- getContents

  let matrix           = id $! buildMatrix allInput
      marioPosition    = fromJust $ findPositionForChar isMario matrix
      princessPosition = fromJust $ findPositionForChar isPrincess matrix
      path             = pathFromMarioToPrincess marioPosition princessPosition

  printDirections path

  where
    printDirections = mapM_ (putStrLn . format)
    format direction = case direction of
      MoveLeft  -> "LEFT"
      MoveRight -> "RIGHT"
      MoveUp    -> "UP"
      MoveDown  -> "DOWN"

pathFromMarioToPrincess :: Position -> Position -> Path
pathFromMarioToPrincess (my, mx) (py, px) = leftRightSteps ++ upDownSteps
  where
    leftOrRight = if mx > px then MoveLeft else MoveRight
    upOrDown = if my > py then MoveUp else MoveDown
    stepsLeftRight = abs $ mx - px
    stepsUpDown = abs $ my - py
    leftRightSteps = take stepsLeftRight (repeat leftOrRight)
    upDownSteps = take stepsUpDown (repeat upOrDown)

buildMatrix :: String -> Matrix
buildMatrix allInput = concatMap withXYIndex (withIndex onlyMatrixLines)
  where
    -- We don't care about the first line, which is a number
    onlyMatrixLines = tail (lines allInput)
    withIndex = zip [0..]

    withXYIndex :: (Int, String) -> [(Position, Char)]
    withXYIndex (yIndex, line) = map (\(xIndex, char) -> ((yIndex, xIndex), char)) (withIndex line)

findPositionForChar :: Finder -> Matrix -> Maybe Position
findPositionForChar finder matrix = do
  (position, _) <- find (\(_, char) -> finder char) matrix
  return position

isMario :: Finder
isMario = (== 'm')

isPrincess :: Finder
isPrincess = (== 'p')

