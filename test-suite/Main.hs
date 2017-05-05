{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Test.Tasty.QuickCheck

import Data.Functor.Identity
import Data.Vector ((!))
import qualified Data.Vector as V
import Numeric.IEEE
import Prelude hiding ((!))

import WaveToy1

default (Int)

main :: IO ()
main = do
  test <- testSpec "WaveToy1" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec =
  parallel $ do
    specCell
    specGrid

specCell :: Spec
specCell =
  parallel $ do
    describe "initCell" $ do
      it "is not all zero" $
        property $ \t x ->
          let c = initCell (t, x) :: Cell Double
          in u c /= 0 || rho c /= 0 || vx c /= 0
    describe "errorCell" $ do
      it "is zero when exact" $
        property $ \t x ->
          let c = initCell (t, x) :: Cell Double
              e = errorCell (t, x) c
          in e == Cell 0 0 0
      it "is not all zero when inexact" $
        property $ \t x t' x' ->
          (t /= t' || x /= x') ==>
          let c = initCell (t, x) :: Cell Double
              e = errorCell (t', x') c
          in e /= Cell 0 0 0
    describe "energyCell" $ do
      it "is zero for vacuum" $
        property $ \u ->
          let c = Cell u 0 0 :: Cell Double
              e = energyCell c
          in e == 0
      it "has the correct potential term" $
        property $ \rho ->
          let c = Cell 0 rho 0 :: Cell Double
              e = energyCell c
          in e == 0.5 * rho ^ 2
      it "has the correct kinetic term" $
        property $ \vx ->
          let c = Cell 0 0 vx :: Cell Double
              e = energyCell c
          in e == 0.5 * vx ^ 2
    describe "rhsCel" $ do
      it "is not all zero" $
        property $ \t x dx ->
          (dx > 0) ==>
          let lb = initCell (t, x - dx)
              ub = initCell (t, x + dx)
              c = initCell (t, x) :: Cell Double
              r = rhsCell (2 * dx) (lb, ub) c
          in u r /= 0 || rho r /= 0 || vx r /= 0

specGrid :: Spec
specGrid =
  parallel $ do
    describe "skeletonGrid" $ do
      it "has the right extent" $
        property $ \xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np :: Grid Double ()
          in bnds skel == (xmin, xmax)
      it "has the right size" $
        property $ \xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np :: Grid Double ()
          in length (cells skel) == np
    describe "coordGrid" $ do
      it "is monotonic" $
        property $ \xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel :: Grid Double Double
              cs = cells coords
          in all id [cs ! i > cs ! (i - 1) | i <- [1 .. V.length cs - 1]]
    describe "integralGrid" $ do
      it "is linear" $
        property $ \alpha beta xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel :: Grid Double Double
              int = integralGrid $ fmap (\x -> alpha * x + beta) coords
              good_int =
                alpha * (xmax ^ 2 - xmin ^ 2) / 2 + beta * (xmax - xmin)
              scale =
                absmaximum
                  [ alpha
                  , beta
                  , xmin
                  , xmax
                  , xmin ^ 2
                  , xmax ^ 2
                  , alpha * xmin ^ 2
                  , alpha * xmax ^ 2
                  , beta * xmin
                  , beta * xmax
                  ]
          in approxEq scale int good_int
    describe "normGrid" $ do
      it "is positive" $
        property $ \alpha xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel :: Grid Double Double
              norm = normGrid $ fmap (\x -> Identity (alpha * x)) coords
          in norm >= 0
      it "is linear" $
        property $ \beta xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel :: Grid Double Double
              norm = normGrid $ fmap (const $ Identity beta) coords
                    -- xnorm = sqrt (1/3 * (xmax^3 - xmin^3) / (xmax - xmin)) 
              scale = absmaximum [beta ^ 2]
          in approxEq scale norm (abs beta)
    describe "initGrid" $ do
      it "has the right time" $
        property $ \t xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel
              g = initGrid t coords :: Grid Double (Cell Double)
          in time g == t
    describe "errorGrid" $ do
      it "is zero when exact" $
        property $ \t xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel
              g = initGrid t coords :: Grid Double (Cell Double)
              err = errorGrid g
          in all (all (~~ 0)) err
      it "is not all zero when inexact" $
        property $ \t t' xmin xmax np ->
          (t /= t' && np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel
              g = initGrid t coords :: Grid Double (Cell Double)
              g' = initGrid t' coords
              err = errorGrid (g {cells = cells g'})
          in not $ all (all (~~ 0)) err
    describe "energyGrid" $ do
      it "is positive" $
        property $ \t xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel
              g = initGrid t coords :: Grid Double (Cell Double)
              etot = integralGrid $ energyGrid g
          in etot > 0
    describe "rhsGrid" $ do
      it "does stuff" $
        property $ \t xmin xmax np ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel
              g = initGrid t coords :: Grid Double (Cell Double)
              bcs = bcGrid g
              r = rhsGrid bcs g
              norm = normGrid r
          in norm >= 0
    describe "rk2Grid" $ do
      it "does stuff" $
        property $ \t xmin xmax np dt ->
          (np > 0 && xmax > xmin) ==>
          let skel = skeletonGrid (xmin, xmax) np
              coords = coordGrid skel
              g = initGrid t coords :: Grid Double (Cell Double)
              rhs g = rhsGrid (bcGrid g) g
              g' = rk2Grid dt rhs g
              norm = normGrid g'
          in norm >= 0

-- |Approximate floating-point comparison
(~~)
  :: (IEEE a, Num a)
  => a -> a -> Bool
(~~) = approxEq 1

infix 4 ~~

-- |Approximate floating-point comparison
approxEq
  :: (IEEE a, Num a)
  => a -> a -> a -> Bool
approxEq scale x y = abs (x - y) < precision * epsilon * scale'
  where
    precision = 100
    scale' = absmaximum [abs x, abs y, scale]

-- |Maximum of absolute values
absmaximum
  :: (Foldable t, Functor t, Num a, Ord a)
  => t a -> a
absmaximum = maximum . fmap abs
