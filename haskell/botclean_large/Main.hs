-- solution for https://www.hackerrank.com/challenges/botcleanlarge
module Main where

import Data.List
import Data.Maybe
import Control.Monad.Trans.Reader

type Position      = (Int, Int)
newtype Dimensions = Dimensions (Int, Int) -- keep it separate from Position
type Entry         = (Position, Char)
type Matrix        = [Entry]
data Action        = GoUp | GoDown | GoRight | GoLeft | Clean
data Env = Env { envPosition   :: Position,
                 envMatrix     :: Matrix,
                 envDimensions :: Dimensions }

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
  let environment = getPositionAndMatrix input
      nextAction = runReader determineNextAction environment
  putStrLn (show nextAction)

findDirtyEntries :: Reader Env Matrix
findDirtyEntries = do
  Env {envMatrix=matrix} <- ask
  return $ filter (\(_, v) -> v == dirtyValue) matrix

findClosestEntry :: Reader Env Entry
findClosestEntry = do
  Env {envPosition=position} <- ask
  dirtyEntries <- findDirtyEntries
  let (closestEntry:_) = entriesWithCost (calculateCost position) dirtyEntries
  return $ snd closestEntry

  where
    entriesWithCost mapper = sortBy compareCost . map mapper
    calculateCost (x, y) entry@((x', y'), _) = let cost = (abs $ x - x') + (abs $ y - y')
                                        in (cost, entry)
    compareCost (cost1, _) (cost2, _) = cost1 `compare` cost2

currentValue :: Reader Env Char
currentValue = do
  Env {envMatrix=matrix, envPosition=position} <- ask
  let currentEntry = fromJust $ find (\(pos, _) -> pos == position) matrix
      (_, currentValue') = currentEntry
  return currentValue'

closestPosition :: Reader Env Position
closestPosition = do
  (closestPosition', _) <- findClosestEntry
  return closestPosition'

determineNextAction :: Reader Env Action
determineNextAction = do
  value <- currentValue
  Env {envPosition=position} <- ask
  closestPosition' <- closestPosition
  if value == dirtyValue
    then return Clean
    else return (originToDestination position closestPosition')

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

getPositionAndMatrix :: String -> Env
getPositionAndMatrix input =
  let (positionLine:dimensionLine:matrixLines) = lines input
      position   = parseTwoInts positionLine
      dimensions = Dimensions (parseTwoInts dimensionLine)
      matrix     = buildMatrix matrixLines
  in Env position matrix dimensions

  where parseTwoInts line = let ints = map read (words line) :: [Int]
                                (y:x:_) = ints
                            in (x, y)

buildMatrix :: [String] -> Matrix
buildMatrix input = concatMap withXYIndex (withIndex input)
  where
    withIndex = zip [0..]

    withXYIndex :: (Int, String) -> [(Position, Char)]
    withXYIndex (yIndex, line) = map (\(xIndex, char) -> ((xIndex, yIndex), char)) (withIndex line)
