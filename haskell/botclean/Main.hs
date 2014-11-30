module Main where

import Data.List
import Data.Maybe

type Position      = (Int, Int)
type Entry         = (Position, Char)
type EntryWithCost = (Int, Entry)
type Matrix        = [Entry]
data Action        = GoUp | GoDown | GoRight | GoLeft | Clean

instance Show Action where
  show dir = case dir of
    GoUp    -> "UP"
    GoDown  -> "DOWN"
    GoRight -> "RIGHT"
    GoLeft  -> "LEFT"
    Clean   -> "CLEAN"

main :: IO ()
main = do
  input <- getContents
  let (position, matrix) = getPositionAndMatrix input
      nextAction = determineNextAction position matrix
  putStrLn (show nextAction)

findClosestEntry :: Position -> Matrix -> Entry
findClosestEntry (x, y) matrix = snd (head entriesWithCost)
  where
    dirtyEntries = filter (\(_, v) -> v == dirtyValue) matrix
    entriesWithCost = sortBy compareCost $ map calculateCost dirtyEntries
    calculateCost entry@((x', y'), _) = let cost = (abs $ x - x') + (abs $ y - y')
                                                 in (cost, entry)
    compareCost (cost1, _) (cost2, _) = cost1 `compare` cost2


determineNextAction :: Position -> Matrix -> Action
determineNextAction position matrix = nextAction value
  where
    value = let currentEntry = fromJust $ find (\(pos, _) -> pos == position) matrix
                (_, currentValue) = currentEntry in currentValue

    closestPosition = let (closestPosition', _) = findClosestEntry position matrix
                      in closestPosition'

    nextAction v
      | v == dirtyValue = Clean
      | otherwise       = originToDestination position closestPosition

originToDestination :: Position -> Position -> Action
originToDestination origin@(x, y) destination@(x', y')
  | moveHorizontal = if x' < x then GoLeft else GoRight
  | moveVertical   = if y' < y then GoUp   else GoDown
  | otherwise      = error $ "can't find direction from: " ++ (show origin) ++ " to: " ++ (show destination)

  where
    moveHorizontal = x /= x'
    moveVertical   = y /= y'

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
