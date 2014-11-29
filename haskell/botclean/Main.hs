module Main where

import Data.List
import Data.Maybe

type Position = (Int, Int)
type Matrix = [(Position, Char)]

main :: IO ()
main = do
  input <- getContents
  let (position, matrix) = getPositionAndMatrix input
      nextAction = determineNextAction position matrix
  --print position
  --print matrix
  putStrLn nextAction

determineNextAction :: Position -> Matrix -> String
determineNextAction position@(x, y) matrix = nextAction value
  where
    currentCell = fromJust $ find (\(pos, _) -> pos == position) matrix
    value = let (_, currentValue) = currentCell
            in currentValue
    nextAction v
      | v == 'd'                = "CLEAN"
      | x == maxIndex && even y = "DOWN"
      | x == 0 && odd y         = "DOWN"
      | even y                  = "RIGHT"
      | odd y                   = "LEFT"
      | otherwise = error "not sure what to do with matrix: " ++ (show matrix) ++ " position: " ++ (show position) ++ " value: " ++ (show v)

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
