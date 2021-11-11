module Main where

import Control.Comonad
import Grid
import Puzzle

main :: IO ()
main = do
  txt <- readFile "sample.txt"
  let ns   = read <$> (concat $ words <$> lines txt)
  let puzz = fromIntList ns
  print $ until (not . any wasUpdated) (extend stepTile) puzz
