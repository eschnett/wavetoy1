{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module WaveToy1
    ( Cell(..)
    , initCell
    , errorCell
    , energyCell
    , rhsCell
    , bcCell
    , Grid(..)
    , integralGrid
    , normGrid
    , skeletonGrid
    , coordGrid
    , initGrid
    , errorGrid
    , energyGrid
    , rhsGrid
    , bcGrid
    , rk2Grid
    ) where

import Control.Applicative
import Control.Exception.Base
import Data.Monoid
import Data.Vector ((!))
import qualified Data.Vector as V
import Prelude hiding ((!))

default (Int)

-- |A cell holds the scalar wave equation state vector for a single
-- grid point.
data Cell a = Cell
    { u, rho, vx :: a
    } deriving (Eq, Ord, Read, Show, Foldable, Functor)

instance Applicative Cell where
    pure x = Cell x x x
    Cell fu frho fvx <*> Cell u rho vx = Cell (fu u) (frho rho) (fvx vx)

sineCell :: Floating a => (a, a) -> Cell a
sineCell (t, x) =
    Cell
    { u = cos (2 * pi * t) * sin (2 * pi * x)
    , rho = -2 * pi * sin (2 * pi * t) * sin (2 * pi * x)
    , vx = 2 * pi * cos (2 * pi * t) * cos (2 * pi * x)
    }

initCell :: Floating a => (a, a) -> Cell a
initCell = sineCell

errorCell :: Floating a => (a, a) -> Cell a -> Cell a
errorCell (t, x) cell = liftA2 (-) cell (initCell (t, x))

energyCell :: Fractional a => Cell a -> a
energyCell c = 1 / 2 * ((rho c) ^ 2 + (vx c) ^ 2)

rhsCell :: Cell a -> Cell a -> Cell a
rhsCell c cx = Cell (rho c) (vx cx) (rho cx)

-- Reflecting boundaries: u -> -u, rho -> -rho, vx -> vx
bcCell :: Num a => Cell a -> Cell a
bcCell c = Cell (-u c) (-rho c) (vx c)

-- |A grid holds the state vector for the whole simulation domain.
data Grid b a = Grid
    { time :: b
    , bnds :: (b, b)
    , cells :: V.Vector a
    } deriving (Read, Show)

npGrid :: Grid b a -> Int
npGrid g = V.length (cells g)

dxGrid :: Fractional b => Grid b a -> b
dxGrid g = (xmax - xmin) / (fromIntegral np - 3)
  where
    (xmin, xmax) = bnds g
    np = npGrid g

instance Foldable (Grid b) where
    foldMap f g = foldMap f (cells g)

instance Functor (Grid b) where
    fmap f g = g {cells = fmap f (cells g)}

instance Applicative (Grid b) where
    pure x = error "Cannot create Grid with unknown size"
    fg <*> g = g {cells = V.generate (npGrid g) $ \i -> (fs ! i) (xs ! i)}
      where
        fs = cells fg
        xs = cells g

densitizeGrid :: Fractional a => Grid b a -> Grid b a
densitizeGrid g = g {cells = V.imap dens (cells g)}
  where
    dens i x
        | i == 0 || i == np - 1 = 0
    dens i x
        | i == 1 || i == np - 2 = 1 / 2 * x
    dens i x = x
    np = npGrid g

integralGrid :: (Fractional a, RealFrac b) => Grid b a -> a
integralGrid g = realToFrac (dxGrid g) * sum (densitizeGrid g)

normGrid :: (Floating a, Foldable c, RealFrac b) => Grid b (c a) -> a
normGrid g = sqrt (sumsq / cnt)
  where
    sumsq = integralGrid $ fmap (getSum . foldMap (Sum . (^ 2))) g
    cnt = integralGrid $ fmap (fromIntegral . length) g

skeletonGrid :: Num a => (a, a) -> Int -> Grid a ()
skeletonGrid bnds np = assert (np > 3) $ Grid 0 bnds $ V.generate np (const ())

coordGrid :: Fractional a => Grid a b -> Grid a a
coordGrid g = g {cells = V.generate (npGrid g) coords}
  where
    coords i = xmin + dx * (fromIntegral i - 1)
    (xmin, _) = bnds g
    dx = dxGrid g

initGrid :: Floating a => a -> Grid a b -> Grid a (Cell a)
initGrid t g = fmap init (coordGrid g) {time = t}
  where
    init x = initCell (t, x)

errorGrid :: Floating a => Grid a (Cell a) -> Grid a (Cell a)
errorGrid g = error <$> coordGrid g <*> g
  where
    error x c = errorCell (time g, x) c

energyGrid :: Fractional a => Grid a (Cell a) -> Grid a a
energyGrid g = fmap energyCell g

derivGrid ::
       (Applicative c, Fractional a, RealFrac b) => Grid b (c a) -> Grid b (c a)
derivGrid g = g {cells = V.generate np deriv}
  where
    deriv i
        | i == 0 = diff (1 / dx) (cs ! 0) (cs ! 1)
    deriv i
        | i == np - 1 = diff (1 / dx) (cs ! (np - 2)) (cs ! (np - 1))
    deriv i = diff (1 / (2 * dx)) (cs ! (i - 1)) (cs ! (i + 1))
    diff alpha = liftA2 (\xm xp -> realToFrac alpha * (xp - xm))
    cs = cells g
    np = npGrid g
    dx = dxGrid g

rhsGrid :: (Fractional a, RealFrac b) => Grid b (Cell a) -> Grid b (Cell a)
rhsGrid g = rhsCell <$> g <*> derivGrid g

bcGrid :: Num a => Grid b (Cell a) -> Grid b (Cell a)
bcGrid g = g {cells = V.imap bcs (cells g)}
  where
    bcs i c
        | i == 0 = blo
    bcs i c
        | i == np - 1 = bhi
    bcs i c = c
    blo = bcCell $ cells g ! 2
    bhi = bcCell $ cells g ! (np - 3)
    np = npGrid g

rk2Grid ::
       (Fractional a, Applicative c)
    => a
    -> (Grid a (c a) -> Grid a (c a))
    -> Grid a (c a)
    -> Grid a (c a)
rk2Grid dt rhs s0 =
    let r0 = rhs s0
        s1 = step s0 (dt / 2) r0
        r1 = rhs s1
        s2 = step s0 dt r1
    in s2
  where
    step (Grid t bnds state) dt (Grid _ _ rhs) =
        Grid (t + dt) bnds $ V.zipWith (liftA2 step') state rhs
    step' s r = s + dt * r
