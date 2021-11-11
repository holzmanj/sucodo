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

extractRow :: Grid a -> Vector a
extractRow (Grid m (i, _)) = getRow i m

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
