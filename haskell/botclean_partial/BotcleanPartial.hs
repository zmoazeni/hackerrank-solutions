{-# LANGUAGE FlexibleInstances #-}
-- solution for https://www.hackerrank.com/challenges/botcleanv2
--
-- Looks ahead a N steps for the closest nodes
-- Keeps an eye on areas visited
module Main where

import Data.List
import Data.Maybe
import Control.Monad.Trans.Reader
import qualified Data.Map.Strict as M
import System.IO
import Data.Functor
import Control.DeepSeq

type Cost = Int
type Position = (Int, Int)
data Entry = Entry { getPosition :: Position, getValue :: Char } deriving (Show, Eq)
data Path = Path { getPositions :: [Position], getEntry :: Entry, getCost :: Int } deriving (Show, Eq)

type Matrix = [Entry]
data Action = GoUp | GoDown | GoRight | GoLeft | Clean
data Env = Env {
  envPath             :: [Position],
  envEntry            :: Entry,
  envAllDirtyEntries  :: Matrix,
  envMatrix           :: Matrix,
  envCurrentPosition  :: Position,
  envVisitedPositions :: [Position]
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

instance Ord Path where
  compare Path{getCost=cost1} Path{getCost=cost2} = cost1 `compare` cost2

dirtyValue :: Char
dirtyValue = 'd'

obscuredValue :: Char
obscuredValue = 'o'

maxIndex :: Int
maxIndex = 4

maxClosest :: Int
maxClosest = 15

maxLookAheadDepth :: Int
maxLookAheadDepth = 4

fileLogPath :: String
fileLogPath = "botclean.txt"

main :: IO ()
main = do
  input <- getContents

  environment <- getPositionAndMatrix input

  let
    lookAheadPathsWithCost   = runReader lookAhead environment
    maybeTotalCosts          = sumLookAheadCost lookAheadPathsWithCost
    currentPosition          = envCurrentPosition environment
    currentValue             = getValue $ envEntry environment
    closestObscuredPosition' = runReader closestObscuredPosition environment

    action = if currentValue == dirtyValue
               then Clean
               else case maybeTotalCosts of
                 Nothing -> originToDestination currentPosition closestObscuredPosition'
                 Just totalCosts -> let (_, cheapestRoute) = head totalCosts
                                        (_:nextPosition:_) = cheapestRoute
                                    in originToDestination currentPosition nextPosition


  putStrLn (show action)

  withFile fileLogPath AppendMode $ \file -> do
    hPutStrLn file (show currentPosition)

closestObscuredPosition :: Reader Env Position
closestObscuredPosition = do
  Env {envMatrix=matrix, envCurrentPosition=currentPosition, envVisitedPositions=visitedPositions} <- ask
  let obscuredPositions = filter (\Entry{getValue=value, getPosition=position} -> value == obscuredValue && not (position `elem` visitedPositions)) matrix
      sortedPositions   = sort $ map (\Entry{getPosition=position} -> ((calculateCost currentPosition position), position)) obscuredPositions
  return $ snd (head sortedPositions)

lookAhead :: Reader Env [Path]
lookAhead = do
  env@Env {envPath=positions, envAllDirtyEntries=dirtyEntries} <- ask
  let
    currentPosition                    = last positions
    dirtyEntriesWithoutCurrentPosition = filter (\Entry{getPosition=position} -> position /= currentPosition) dirtyEntries
    pathsWithCost                      = sort $ map (\entry@Entry{getPosition=position'} -> Path positions entry (calculateCost currentPosition position')) dirtyEntriesWithoutCurrentPosition
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

sumLookAheadCost :: [Path] -> Maybe [(Int, [Position])]
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

    allPathsWithCost = sort $ map (\path -> (costForPath path, path)) finalPositionsOrOnlyPath
  in if null allPathsWithCost then Nothing else Just allPathsWithCost

originToDestination :: Position -> Position -> Action
originToDestination origin@(x, y) destination@(x', y')
  | moveHorizontal = if x' < x then GoLeft else GoRight
  | moveVertical   = if y' < y then GoUp   else GoDown
  | otherwise      = error $ "can't find direction from: " ++ (show origin) ++ " to: " ++ (show destination)

  where
    moveHorizontal = x /= x'
    moveVertical   = y /= y'

getPositionAndMatrix :: String -> IO Env
getPositionAndMatrix input = do
  let
    (positionLine:matrixLines) = lines input

    position     = parseTwoInts positionLine
    matrix       = buildMatrix matrixLines
    dirtyEntries = filter (\Entry {getValue=v} -> v == dirtyValue) matrix
    currentEntry = fromJust $ find (\Entry {getPosition=position'} -> position == position') matrix

  previousPositions <- withFile fileLogPath ReadWriteMode $ \file -> do
    let fileContents = (hGetContents file)
    previousPositions <- positionLines <$> fileContents
    return $!! previousPositions

  return (Env [position] currentEntry dirtyEntries matrix position previousPositions)

  where
    parseTwoInts line = let ints = map read (words line) :: [Int]
                            (y:x:_) = ints
                        in (x, y)

    positionLines :: String -> [Position]
    positionLines = map (\s -> read s) . lines

buildMatrix :: [String] -> Matrix
buildMatrix input = concatMap withXYIndex (withIndex input)
  where
    withIndex = zip [0..]

    withXYIndex :: (Int, String) -> Matrix
    withXYIndex (yIndex, line) = map (\(xIndex, char) -> Entry (xIndex, yIndex) char) (withIndex line)
