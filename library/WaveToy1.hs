{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module WaveToy1 (Cell(..), initCell, errorCell, energyCell, rhsCell,
                 Grid(..), integralGrid, normGrid,
                 initGrid, errorGrid, energyGrid, rhsGrid, bcGrid, rk2Grid)
where

import Control.Applicative
import Data.Monoid
import Data.Vector ((!))
import Prelude hiding ((!))
import qualified Data.Vector as V

default (Int)



-- |A cell holds the scalar wave equation state vector for a single
-- grid point.
data Cell a = Cell { u, rho, vx :: a }
  deriving (Eq, Ord, Read, Show, Foldable, Functor)

instance Applicative Cell where
  pure x = Cell x x x
  Cell fu frho fvx <*> Cell u rho vx = Cell (fu u) (frho rho) (fvx vx)

sineCell :: Floating a => (a, a) -> Cell a
sineCell (t, x) = Cell { u = cos (2*pi*t) * sin (2*pi*x),
                         rho = -2*pi * sin (2*pi*t) * sin (2*pi*x),
                         vx = 2*pi * cos (2*pi*t) * cos (2*pi*x) }

initCell :: Floating a => (a, a) -> Cell a
initCell = sineCell

errorCell :: Floating a => (a, a) -> Cell a -> Cell a
errorCell (t, x) cell = liftA2 (-) cell (sineCell (t, x))

energyCell :: Fractional a => Cell a -> a
energyCell (Cell u rho vx) = 1/2 * (rho^2 + vx^2)

rhsCell :: Fractional a => a -> (Cell a, Cell a) -> Cell a -> Cell a
rhsCell dx (lb, ub) c = Cell (rho c) (ddx vx) (ddx rho)
  where ddx f = (f ub - f lb) / (2 * dx)

-- reflecting boundaries: u -> -u
flipCell :: Num a => Cell a -> Cell a
flipCell (Cell u rho vx) = Cell (-u) (-rho) vx



-- |A grid holds the state vector for the whole simulation domain.
data Grid b a = Grid { iter :: Int,
                       time :: b,
                       bnds :: (b, b),
                       cells :: V.Vector a }
  deriving (Read, Show)

instance Foldable (Grid b) where
  foldMap f g = foldMap f (cells g)

instance Functor (Grid b) where
  fmap f g = g { cells = fmap f (cells g) }

integralGrid :: Fractional a => Grid a a -> a
integralGrid g = getSum (foldMap Sum (cells g)) * dx
  where dx = (xmax - xmin) / fromIntegral np
        (xmin, xmax) = bnds g
        np = V.length (cells g)

normGrid :: (Floating a, Foldable c) => Grid a (c a) -> a
normGrid g = sqrt (sumsq / cnt)
  where sumsq = getSum $ foldMap (foldMap (Sum . (^2))) (cells g)
        cnt = getSum $ foldMap (foldMap (Sum . const 1)) (cells g)

coords :: Fractional a => (a, a) -> Int -> Int -> a
coords (xmin, xmax) np = \i -> xmin + dx * (fromIntegral i + 1/2)
  where dx = (xmax - xmin) / fromIntegral np

initGrid :: Floating a => a -> (a, a) -> Int -> Grid a (Cell a)
initGrid time bnds np = Grid 0 time bnds $ V.generate np init
  where init i = initCell (time, coords bnds np i)

errorGrid :: Floating a => Grid a (Cell a) -> Grid a (Cell a)
errorGrid g = g { cells = V.imap error (cells g) }
  where error i cell = errorCell (t, x i) cell
        t = time g
        x = coords (bnds g) np
        np = V.length (cells g)

energyGrid :: Fractional a => Grid a (Cell a) -> Grid a a
energyGrid g = g { cells = V.map energyCell (cells g) }

rhsGrid :: Fractional a =>
           (Cell a, Cell a) -> Grid a (Cell a) -> Grid a (Cell a)
rhsGrid (lb, ub) g@(Grid _ _ (xmin, xmax) cs) = g { cells = V.fromList rhs }
  where rhs = rblo ++ rint ++ rbhi
        rblo = [rhsCell dx (lb, cs ! 1) (cs ! 0)]
        rint = [rhsCell dx (cs ! (i-1), cs ! (i+1)) (cs ! i) |
                i <- [1 .. np-2]]
        rbhi = [rhsCell dx (cs ! (np-2), ub) (cs ! (np-1))]
        dx = (xmax - xmin) / fromIntegral np
        np = length cs

bcGrid :: Num a => Grid b (Cell a) -> (Cell a, Cell a)
bcGrid g = (flipCell blo, flipCell bhi)
  where blo = cells g ! 0
        bhi = cells g ! (np-1)
        np = length (cells g)

rk2Grid :: (Fractional a, Applicative c) =>
           a -> (Grid a (c a) -> Grid a (c a)) -> Grid a (c a) -> Grid a (c a)
rk2Grid dt rhs s0 =
  let r0 = rhs s0
      s1 = step s0 (dt/2) r0
      r1 = rhs s1
      s2 = step s0 dt r1
  in s2
  where step (Grid it t bnds state) dt (Grid _ _ _ rhs) =
          Grid it (t + dt) bnds $ V.zipWith (liftA2 step') state rhs
        step' s r = s + dt * r
