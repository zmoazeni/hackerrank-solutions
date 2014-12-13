import Control.Exception
import Data.Functor
import System.IO
import Control.DeepSeq

type Position = (Int, Int)

filePath :: String
filePath = "position.txt"

main :: IO ()
main = do
  contents <- withFile filePath ReadWriteMode $ \file -> do
    let fileContents = (hGetContents file) `catch` nada
    stuff <- positions . lines <$> fileContents
    return $!! stuff

  print contents

  newPosition <- getInputPosition
  print newPosition

  withFile filePath AppendMode (appendPosition newPosition)



nada :: IOException -> IO String
nada _ = return ""

positions :: [String] -> [Position]
positions = map (\s -> read s)

getInputPosition :: IO Position
getInputPosition = read . head . lines <$> getContents

appendPosition :: Position -> Handle -> IO ()
appendPosition position file = hPutStrLn file (show position)

