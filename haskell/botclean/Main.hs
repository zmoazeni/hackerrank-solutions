module Main where

import Data.List
import Data.Maybe

type Position = (Int, Int)
type Entry = (Position, Char)
type Matrix = [Entry]

data Action = GoDown | GoRight | GoLeft | Clean

instance Show Action where
  show dir = case dir of
    GoDown  -> "DOWN"
    GoRight -> "RIGHT"
    GoLeft  -> "LEFT"
    Clean   -> "CLEAN"

main :: IO ()
main = do
  input <- getContents
  let (position, matrix) = getPositionAndMatrix input
      nextAction = determineNextAction position matrix
  --print position
  --print matrix
  putStrLn (show nextAction)

determineNextAction :: Position -> Matrix -> Action
determineNextAction position@(x, y) matrix = nextAction value
  where
    value = let currentEntry = fromJust $ find (\(pos, _) -> pos == position) matrix
                (_, currentValue) = currentEntry in currentValue

    canOptimize = not $ any (\entry ->
        dirtyCellOnCurrentRow position entry || willMissDirtyCellOnNextRow position entry
      ) matrix

    nextAction v
      | v == dirtyValue         = Clean
      | canOptimize             = GoDown
      | x == maxIndex && even y = GoDown
      | x == 0 && odd y         = GoDown
      | even y                  = GoRight
      | odd y                   = GoLeft
      | otherwise = error $ "not sure what to do with matrix: " ++ (show matrix) ++ " position: " ++ (show position) ++ " value: " ++ (show v)

dirtyCellOnCurrentRow :: Position -> Entry -> Bool
dirtyCellOnCurrentRow (_, y) ((_, y'), value) = y == y' && value == dirtyValue

willMissDirtyCellOnNextRow :: Position -> Entry -> Bool
willMissDirtyCellOnNextRow (x, y) ((x', y'), value) = otherDirtyCells && (y + 1) == y' && value == dirtyValue
  where otherDirtyCells = if odd y' then x' < x else x' > x

dirtyValue :: Char
dirtyValue = 'd'

maxIndex :: Int
maxIndex = 4

getPositionAndMatrix :: String -> (Position, Matrix)
getPositionAndMatrix input =
  let (firstLine:rest) = lines input
      position = getPosition firstLine
      matrix = buildMatrix rest
  in (position, matrix)

  where getPosition line = let ints = map read (words line) :: [Int]
                               (y:x:_) = ints
                           in (x, y)

buildMatrix :: [String] -> Matrix
buildMatrix input = concatMap withXYIndex (withIndex input)
  where
    withIndex = zip [0..]

    withXYIndex :: (Int, String) -> [(Position, Char)]
    withXYIndex (yIndex, line) = map (\(xIndex, char) -> ((xIndex, yIndex), char)) (withIndex line)
