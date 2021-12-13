module Grid where

import Control.Comonad
import Data.Matrix ((!), Matrix(..), getCol, getRow, matrix, submatrix)
import Data.Vector (Vector(..))

data Grid a = Grid (Matrix a) (Int, Int)
  deriving Eq

instance (Show a) => Show (Grid a) where
  show (Grid m _) = show m

instance Functor Grid where
  fmap f (Grid m p) = Grid (fmap f m) p

instance Comonad Grid where
  extract (Grid m p) = m ! p
  extend f (Grid m p) =
    let
      gen (i, j) = f $ Grid m (i, j)
      m' = matrix (nrows m) (ncols m) gen
    in Grid m' p

instance Foldable Grid where
  foldMap f (Grid m _) = foldMap f m

instance Traversable Grid where
  sequenceA (Grid fm p) = (`Grid` p) <$> sequenceA fm

-- | Extract all values in the same row as the focused cell.
extractRow :: Grid a -> Vector a
extractRow (Grid m (i, _)) = getRow i m

-- | Extract all values in the same column as the focused cell.
extractCol :: Grid a -> Vector a
extractCol (Grid m (_, j)) = getCol j m

-- | Splits matrix up into regions of a given size and returns the region
-- containing the Grid's position.
extractBox :: (Int, Int) -> Grid a -> Matrix a
extractBox (h, w) (Grid m (y, x)) = submatrix (r0 + 1) r1 (c0 + 1) c1 m
 where
  r0 = ((y - 1) `div` h) * h
  r1 = r0 + h
  c0 = ((x - 1) `div` w) * w
  c1 = c0 + w

-- | Get a list of all rows in the grid
allRows :: Grid a -> [Vector a]
allRows (Grid m _) = flip getRow m <$> [1 .. nrows m]

-- | Get a list of all columns in the grid
allCols :: Grid a -> [Vector a]
allCols (Grid m _) = flip getRow m <$> [1 .. ncols m]

-- | Get a list of all boxes of a given size within the grid
allBoxes :: (Int, Int) -> Grid a -> [Matrix a]
allBoxes (h, w) (Grid m _) =
  let
    box y x = submatrix (y * h + 1) ((y + 1) * h) (x * w + 1) ((x + 1) * w) m
    nx = ncols m `div` w
    ny = nrows m `div` h
  in box <$> [0 .. ny - 1] <*> [0 .. nx - 1]

-- | Sets the focused cell to the top-left most one.
focusFirstCell :: Grid a -> Grid a
focusFirstCell (Grid m _) = Grid m (1, 1)

-- | Moves the focused cell one step to the right, or to the beginning
-- of the next row if at the end of the current row.
advanceCell :: Grid a -> Grid a
advanceCell g@(Grid m (x, y))
  | isLastCell g = Grid m (1, 1)
  | x == ncols m = Grid m (1, y + 1)
  | otherwise    = Grid m (x + 1, y)

-- | Checks if the grid's focused cell is the bottom-right most one.
isLastCell :: Grid a -> Bool
isLastCell (Grid m (x, y)) = x == ncols m && y == nrows m
