{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Comonad
import Data.List
import Data.Time
import Grid
import Control.Monad
import Options.Applicative
import Puzzle
import System.Environment
import Text.Printf

newtype Options = Options
  { optCommand :: Command
  }

data Command
  = Solve FilePath
  | Bench FilePath


getFilePath :: Parser FilePath
getFilePath = argument
  str
  (metavar "FILE" <> help "File containing one or more sudoku puzzles.")

options :: Parser Options
options = Options <$> subparser
  (  command
      "solve"
      (info
        (Solve <$> getFilePath)
        (progDesc "Solve a puzzle and print the solution.")
      )
  <> command
      "bench"
      (info
        (Bench <$> getFilePath)
        (progDesc "Measure performance while solving all puzzles in a file.")
      )
  )

optInfo :: ParserInfo Options
optInfo = info
  (options <**> helper)
  (fullDesc <> header "SuCodo - comonadic sudoku solver")

stripInput :: String -> String
stripInput s =
  let s' = unlines $ filter (not . isPrefixOf "#") (lines s)
  in filter (`elem` ('.' : ['0' .. '9'])) s'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l  = (take n l) : (chunksOf n (drop n l))


diffToFloat :: NominalDiffTime -> Float
diffToFloat dt = fromRational $ toRational dt


main :: IO ()
main = do
  opts <- execParser optInfo
  case optCommand opts of
    Solve path -> do
      puzz <- (fromString . stripInput) <$> (readFile path)
      case solve puzz of
        Nothing    -> putStrLn "Puzzle is unsolvable"
        Just puzz' -> print puzz'
    Bench path -> do
      txt <- stripInput <$> readFile path
      let !puzzles = fromString <$> (chunksOf 81 txt)
      t0 <- getCurrentTime
      let t = t0 `seq` foldl' (\_ p -> solve p) Nothing puzzles
      t1 <- t `seq` getCurrentTime
      let dt = diffToFloat $ diffUTCTime t1 t0
      printf "Solved %d puzzles in %.3fs" (length puzzles) dt
      printf " (%.3f puzzles/sec)" $ (fromIntegral $ length puzzles) / dt



