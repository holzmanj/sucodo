module Main where

import Control.Comonad
import Grid
import Puzzle
import System.Environment

main :: IO ()
main = do
  as <- getArgs
  if null as
    then putStrLn "Need path to sudoku puzzle"
    else do
      txt <- readFile (head as)
      let ns   = read <$> concat (words <$> lines txt)
      let puzz = fromIntList ns
      print $ until (not . any wasUpdated) (extend stepTile) puzz
