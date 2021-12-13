module Puzzle where

import Control.Comonad (extend, extract)
import Data.Foldable (asum)
import Data.List ((\\))
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

-- | Read a puzzle from a string containing the numbers 1 through 9 for cells
-- that are filled in, and a 0 or '.' for cells that are empty.  All other
-- characters are skipped over.
fromString :: String -> Puzzle
fromString s = Grid (M.fromList 9 9 tiles) (1, 1)
 where
  tiles = (`Tile` True) <$> take 81 (strToVals s)
  strToVals [] = error "Not enough elements!"
  strToVals (c : cs)
    | c `elem` ['1' .. '9'] = Definitely (read $ pure c) : strToVals cs
    | c == '0' || c == '.'  = Possibly [1 .. 9] : strToVals cs
    | otherwise             = strToVals cs

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

-- | Propagate the constraints of possible values for the focused tile based on
-- the existing values in the same row, column, and box.  If a contradiction is
-- found and the tile cannot be given any value, returns @Nothing@.
stepTile :: Puzzle -> Maybe Tile
stepTile puzz = case val of
  Definitely _  -> Just (Tile val False)
  Possibly   ps -> case ps \\ existing of
    []  -> Nothing
    [i] -> mkTile (Definitely i)
    ps' -> mkTile (Possibly ps')
 where
  val      = tileVal $ extract puzz
  existing = catDefinitely (tileVal <$> tilesToCheck puzz)
  mkTile newV = Just (Tile newV (val /= newV))

-- | Find the first tile with multiple possibilities and branch into a list of
-- possible puzzles where each assumes one of those possibile tiles to be
-- definite.  If all tiles are definite, return a single definite puzzle.
branch :: Puzzle -> Uncertain Puzzle
branch puzz = branch' (focusFirstCell puzz)
 where
  branch' pz = case tileVal (extract pz) of
    Definitely _ ->
      if isLastCell pz then Definitely pz else branch' (advanceCell pz)
    Possibly xs -> Possibly (setDef pz <$> xs)
  -- set the focused tile to some definite value
  setDef (Grid m p) v = Grid (M.setElem (Tile (Definitely v) True) p m) p

-- | Attempt to solve the given puzzle.  If unsolvable, @Nothing@ is returned
-- instead.
solve :: Puzzle -> Maybe Puzzle
solve puzz = do
  puzz' <- sequenceA (extend stepTile puzz)
  if any wasUpdated puzz'
    then solve puzz'
    else case branch puzz' of
      Definitely b  -> if validSolution b then Just b else Nothing
      Possibly   bs -> asum (solve <$> bs)

-- | Checks if a given puzzle contains a valid solution, meaning each row,
-- column, and box contains the numbers 1 through 9.
validSolution :: Puzzle -> Bool
validSolution puzz =
  let
    groups = V.toList <$> concat
      [allRows puzz, allCols puzz, M.getMatrixAsVector <$> allBoxes (3, 3) puzz]
    groupValid g = null $ (tileVal <$> g) \\ (Definitely <$> [1 .. 9])
  in all groupValid groups
