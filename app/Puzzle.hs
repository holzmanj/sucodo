module Puzzle where

import Control.Comonad (extract)
import Data.List (delete, foldl')
import qualified Data.Matrix as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Grid

data Uncertain a
  = Possibly [a]
  | Definitely a
  deriving (Eq)

instance (Show a) => Show (Uncertain a) where
  show (Possibly   as) = "?"
  show (Definitely a ) = show a

data Tile = Tile
  { tileVal    :: Uncertain Int
  , wasUpdated :: Bool
  }
  deriving Eq

instance Show Tile where
  show t = show $ tileVal t

type Puzzle = Grid Tile

fromIntList :: [Int] -> Puzzle
fromIntList xs =
  let
    toTile n = Tile (if n == 0 then Possibly [1 .. 9] else Definitely n) True
    m = M.fromList 9 9 (toTile <$> xs)
  in if length xs < 81 then error "Not enough elements!" else Grid m (1, 1)

catDefinitely :: [Uncertain a] -> [a]
catDefinitely [] = []
catDefinitely (x : xs) =
  let ys = catDefinitely xs
  in
    case x of
      Definitely y -> y : ys
      _            -> ys

tilesToCheck :: Puzzle -> [Tile]
tilesToCheck puzz =
  let
    row = extractRow puzz
    col = extractCol puzz
    box = M.getMatrixAsVector (extractBox (3, 3) puzz)
  in V.toList $ row V.++ col V.++ box

stepTile :: Puzzle -> Tile
stepTile puzz = case val of
  Definitely _ -> Tile val False
  Possibly ps ->
    let
      ps'  = foldl' (flip delete) ps existing
      val' = case ps' of
        []  -> error "Invalid puzzle, cannot be solved."
        [i] -> Definitely i
        _   -> Possibly ps'
    in Tile val' (val /= val')
 where
  val      = tileVal $ extract puzz
  existing = catDefinitely (tileVal <$> tilesToCheck puzz)

