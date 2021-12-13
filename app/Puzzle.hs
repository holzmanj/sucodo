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

-- | Create a sudoku puzzle from a list of integers.  List should contain 81
-- elements in order from left to right, top to bottom, each being a number from
-- 0 to 9 where 0 is an empty cell.
fromIntList :: [Int] -> Puzzle
fromIntList xs =
  let
    toTile n = Tile (if n == 0 then Possibly [1 .. 9] else Definitely n) True
    m = M.fromList 9 9 (toTile <$> xs)
  in if length xs < 81 then error "Not enough elements!" else Grid m (1, 1)

-- | Reduce a list of `Uncertain` values into only those which are definite.
catDefinitely :: [Uncertain a] -> [a]
catDefinitely [] = []
catDefinitely (x : xs) =
  let ys = catDefinitely xs
  in
    case x of
      Definitely y -> y : ys
      _            -> ys

-- | Extract a list of all of the `Tile`s in the same row, column, and box as
-- the `Puzzle`'s focused cell.
tilesToCheck :: Puzzle -> [Tile]
tilesToCheck puzz =
  let
    row = extractRow puzz
    col = extractCol puzz
    box = M.getMatrixAsVector (extractBox (3, 3) puzz)
  in V.toList $ row V.++ col V.++ box

-- | Restrict the possibilities of the focused tile by getting rid of values
-- which already definitely exist in the same row, column, and box.
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

-- | Find the first tile with multiple possibilities and branch into a list of
-- puzzles where each assumes one of those possibilities to be definite.
--
-- If all tiles are definite, return a singleton list with the puzzle unchanged.
branch :: Puzzle -> [Puzzle]
branch puzz = branch' (focusFirstCell puzz)
 where
  branch' pz@(Grid m pos) = case tileVal (extract pz) of
    Definitely _  -> if isLastCell pz then [pz] else branch' (advanceCell pz)
    Possibly   xs -> setDef pz <$> xs
  -- set the focused tile to some definite value
  setDef (Grid m p) v = Grid (M.setElem (Tile (Definitely v) False) p m) p

