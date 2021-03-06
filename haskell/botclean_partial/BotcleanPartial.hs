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
import Debug.Trace

type Cost = Int
type Position = (Int, Int)
data Entry = Entry { getPosition :: Position, getValue :: Char } deriving (Show, Eq)
data Path = Path { getPositions :: [Position], getEntry :: Entry, getCost :: Int } deriving (Show, Eq)

type Matrix = [Entry]
data Action = GoUp | GoDown | GoRight | GoLeft | Clean
data Env = Env {
  envPath                 :: [Position],
  envEntry                :: Entry,
  envAllDirtyEntries      :: Matrix,
  envMatrix               :: Matrix,
  envCurrentPosition      :: Position,
  envKnownBlankPositions  :: [Position],
  envPassedDirtyPositions :: [Position]
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

blankValue :: Char
blankValue = '-'

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
    lookAheadPathsWithCost = runReader lookAhead environment
    maybeTotalCosts        = sumLookAheadCost lookAheadPathsWithCost
    currentPosition        = envCurrentPosition environment
    currentValue           = getValue $ envEntry environment
    blankPositions         = runReader allKnownBlankPositions environment
    dirtyPositions         = runReader (allKnownDirtyPositions blankPositions) environment
    nextObscuredPosition   = runReader closestObscuredPosition environment{envKnownBlankPositions=blankPositions, envPassedDirtyPositions=dirtyPositions}

    nextPosition = case maybeTotalCosts of
                     Nothing -> nextObscuredPosition
                     Just totalCosts -> let (_, cheapestRoute) = head totalCosts
                                            (_:cheapestDirtyPosition:_) = cheapestRoute
                                        in cheapestDirtyPosition

    action = if currentValue == dirtyValue
               then Clean
               else originToDestination currentPosition nextPosition

  putStrLn (show action)

  withFile fileLogPath WriteMode $ \file -> do
    let toSave = (blankPositions, dirtyPositions)
    hPutStrLn file (show toSave)

allKnownBlankPositions :: Reader Env [Position]
allKnownBlankPositions = do
  Env {envMatrix=matrix, envKnownBlankPositions=knownBlankPositions} <- ask
  let visibleBlankPositions = map getPosition $ filter (\Entry{getValue=value} -> value == blankValue) matrix
  return $ nub (knownBlankPositions ++ visibleBlankPositions)

allKnownDirtyPositions :: [Position] -> Reader Env [Position]
allKnownDirtyPositions blankPositions = do
  Env {envMatrix=matrix, envPassedDirtyPositions=knownDirtyPositions} <- ask
  let visibleDirty = map getPosition $ filter (\Entry{getValue=value} -> value == dirtyValue) matrix
      knownDirtyPositions' = filter (\pos -> not $ pos `elem` blankPositions) knownDirtyPositions
  return $ nub (knownDirtyPositions' ++ visibleDirty)

closestObscuredPosition :: Reader Env Position
closestObscuredPosition = do
  Env {envMatrix=matrix,
       envCurrentPosition=currentPosition,
       envKnownBlankPositions=blankPositions,
       envPassedDirtyPositions=passedDirtyPositions} <- ask
  let
      sortedPositions       = sort . map (\position -> ((calculateCost currentPosition position), position))
      obscuredPositions     = sortedPositions $ map getPosition $ filter (\Entry{getValue=value, getPosition=position} -> value == obscuredValue && not (position `elem` blankPositions)) matrix
      passedDirtyPositions' = sortedPositions $ filter (/= currentPosition) passedDirtyPositions
      closestPosition       = snd . head $ if null passedDirtyPositions' then obscuredPositions else passedDirtyPositions'
  return closestPosition

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

  (blankPositions, passedDirtyPositions) <- withFile fileLogPath ReadWriteMode $ \file -> do
    fileContents <- (hGetContents file)
    hPutStr stderr fileContents
    let previousPosAndDirtyPos = positionLines fileContents
    return $!! previousPosAndDirtyPos

  return $ Env {
    envPath=[position],
    envEntry=currentEntry,
    envAllDirtyEntries=dirtyEntries,
    envMatrix=matrix,
    envCurrentPosition=position,
    envKnownBlankPositions=blankPositions,
    envPassedDirtyPositions=passedDirtyPositions
  }

  where
    parseTwoInts line = let ints = map read (words line) :: [Int]
                            (y:x:_) = ints
                        in (x, y)

    positionLines :: String -> ([Position], [Position])
    positionLines input = let parsed = map read (lines input)
                              empty = ([], [])
                          in if null parsed then empty else head parsed

buildMatrix :: [String] -> Matrix
buildMatrix input = concatMap withXYIndex (withIndex input)
  where
    withIndex = zip [0..]

    withXYIndex :: (Int, String) -> Matrix
    withXYIndex (yIndex, line) = map (\(xIndex, char) -> Entry (xIndex, yIndex) char) (withIndex line)
