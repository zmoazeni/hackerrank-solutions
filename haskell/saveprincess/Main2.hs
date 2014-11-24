-- saveprincess2 challenge https://www.hackerrank.com/challenges/saveprincess2

module Main where

import Data.List
import Data.Maybe
import Data.Char

type Position = (Int, Int)
type Matrix = [(Position, Char)]
type Finder = Char -> Bool
type Path = [Direction]

data Direction = MoveLeft | MoveRight | MoveUp | MoveDown
  deriving (Show)

main :: IO ()
main = do
  allInput <- getContents

  let (matrix, marioPosition)  = buildMatrix allInput
      princessPosition = fromJust $ findPositionForChar isPrincess matrix
      path             = pathFromMarioToPrincess marioPosition princessPosition

  printNextDirection path

  where
    printNextDirection = putStrLn . format . head
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

buildMatrix :: String -> (Matrix, Position)
buildMatrix allInput = (matrix, marioPosition)
  where
    matrix = concatMap withXYIndex (withIndex onlyMatrixLines)
    marioPosition = parsePosition marioPositionLine

    inputLines = lines allInput
    -- We don't care about the first line, which is a number
    onlyMatrixLines = drop 2 inputLines
    marioPositionLine = (head . drop 1) inputLines

    withIndex = zip [0..]

    withXYIndex :: (Int, String) -> [(Position, Char)]
    withXYIndex (yIndex, line) = map (\(xIndex, char) -> ((yIndex, xIndex), char)) (withIndex line)

parsePosition :: String -> Position
parsePosition line = let (yString, xString) = getPositionStrings
  in (parseInt yString, parseInt xString)
  where
    getPositionStrings = break isSpace line
    parseInt = read . filter (not . isSpace)


findPositionForChar :: Finder -> Matrix -> Maybe Position
findPositionForChar finder matrix = do
  (position, _) <- find (\(_, char) -> finder char) matrix
  return position

isPrincess :: Finder
isPrincess = (== 'p')
