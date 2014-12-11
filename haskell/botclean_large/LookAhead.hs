{-# LANGUAGE FlexibleInstances #-}
-- solution for https://www.hackerrank.com/challenges/botcleanlarge
--
-- Looks ahead a N steps for the closest nodes
module Main where

import Data.List
import Data.Maybe
import Debug.Trace
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M

type Cost = Int
type Position = (Int, Int)
data Entry = Entry { getPosition :: Position, getValue :: Char } deriving (Show, Eq)
data Path = Path { getPositions :: [Position], getEntry :: Entry, getCost :: Int } deriving (Show)

type Matrix = [Entry]
data Action = GoUp | GoDown | GoRight | GoLeft | Clean
data Env = Env {
  envPath            :: [Position],
  envEntry           :: Entry,
  envAllDirtyEntries :: Matrix
} deriving (Show)

instance Show Action where
  show dir = case dir of
    GoUp    -> "UP"
    GoDown  -> "DOWN"
    GoRight -> "RIGHT"
    GoLeft  -> "LEFT"
    Clean   -> "CLEAN"

class CostCalculatable a where
  calculateCost :: a -> a -> Cost

instance CostCalculatable Position where
  calculateCost (x, y) (x', y') = (abs $ x - x') + (abs $ y - y')

instance CostCalculatable Entry where
  calculateCost Entry {getPosition=position} Entry {getPosition=position'} = calculateCost position position'

dirtyValue :: Char
dirtyValue = 'd'

maxIndex :: Int
maxIndex = 4

maxClosest :: Int
maxClosest = 10

maxLookAheadDepth :: Int
maxLookAheadDepth = 10

main :: IO ()
main = do
  input <- getContents
  let
    environment                      = getPositionAndMatrix input
    lookAheadPathsWithCost           = runReader lookAhead environment
    totalCosts                       = sumLookAheadCost lookAheadPathsWithCost
    (cheapestRoute, _)               = head totalCosts
    (currentPosition:nextPosition:_) = cheapestRoute
    currentValue                     = getValue $ envEntry environment
    action = if currentValue == dirtyValue
               then Clean
               else originToDestination currentPosition nextPosition
  putStrLn (show action)

lookAhead :: Reader Env [Path]
lookAhead = do
  env@Env {envPath=positions, envAllDirtyEntries=dirtyEntries} <- ask
  let
    currentPosition                    = last positions
    dirtyEntriesWithoutCurrentPosition = filter (\Entry{getPosition=position} -> position /= currentPosition) dirtyEntries
    pathsWithCost                      = sortBy compareCost $ map (\entry@Entry{getPosition=position'} -> Path positions entry (calculateCost currentPosition position')) dirtyEntriesWithoutCurrentPosition
    closestPaths                       = take maxClosest pathsWithCost

  if (length positions) >= maxLookAheadDepth
    then return closestPaths -- don't look anymore, return paths with cost
    else do
      let otherPaths = concatMap (\path ->
                                    let
                                      position = getPosition $ getEntry path
                                      newEnv = env {envPath=positions ++ [position], envAllDirtyEntries=dirtyEntriesWithoutCurrentPosition}
                                    in runReader lookAhead newEnv) closestPaths
      return (closestPaths ++ otherPaths)

  where compareCost Path{getCost=cost1} Path{getCost=cost2} = cost1 `compare` cost2

sumLookAheadCost :: [Path] -> [([Position], Int)]
sumLookAheadCost paths =
  let
    costMap :: M.Map [Position] Int
    costMap = M.fromList $ map (\Path{getPositions=positions, getEntry=entry, getCost=cost} -> (positions ++ [getPosition entry], cost)) paths

    finalPositions :: [[Position]]
    finalPositions = filter ((== maxLookAheadDepth) . length) $ M.keys costMap

    finalPositionsOrOnlyPath = if null finalPositions then M.keys costMap else finalPositions

    costForPath :: [Position] -> Int
    costForPath path =
      let
        lookups = map (\howMany -> take howMany path) [2..maxLookAheadDepth]
        allCosts = map (\key -> fromJust $ M.lookup key costMap) lookups
      in sum allCosts

    allPathsWithCost = map (\path -> (path, costForPath path)) finalPositionsOrOnlyPath

  in sortBy (\(_, cost1) (_, cost2) -> cost1 `compare` cost2 ) allPathsWithCost

originToDestination :: Position -> Position -> Action
originToDestination origin@(x, y) destination@(x', y')
  | moveHorizontal = if x' < x then GoLeft else GoRight
  | moveVertical   = if y' < y then GoUp   else GoDown
  | otherwise      = error $ "can't find direction from: " ++ (show origin) ++ " to: " ++ (show destination)

  where
    moveHorizontal = x /= x'
    moveVertical   = y /= y'

getPositionAndMatrix :: String -> Env
getPositionAndMatrix input =
  let
    (positionLine:_:matrixLines) = lines input

    position     = parseTwoInts positionLine
    matrix       = buildMatrix matrixLines
    dirtyEntries = filter (\Entry {getValue=v} -> v == dirtyValue) matrix
    currentEntry = fromJust $ find (\Entry {getPosition=position'} -> position == position') matrix
  in Env [position] currentEntry dirtyEntries
  where parseTwoInts line = let ints = map read (words line) :: [Int]
                                (y:x:_) = ints
                            in (x, y)

buildMatrix :: [String] -> Matrix
buildMatrix input = concatMap withXYIndex (withIndex input)
  where
    withIndex = zip [0..]

    withXYIndex :: (Int, String) -> Matrix
    withXYIndex (yIndex, line) = map (\(xIndex, char) -> Entry (xIndex, yIndex) char) (withIndex line)
