{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Control.Monad
import Control.Monad.Loops

import WaveToy1

-- Parameters
tini :: Double
tini = 0

xmin :: Double
xmin = 0

xmax :: Double
xmax = 1

np :: Int
np = 20

dx :: Double
dx = (xmax - xmin) / fromIntegral np

alpha :: Double
alpha = 1 / 2

dt :: Double
dt = alpha * dx

niters :: Int
niters = round (fromIntegral np / alpha)

out_every :: Int
out_every = niters `div` 10

iterateWhileM_
  :: Monad m
  => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateWhileM_ predicate action state = do
  _ <- iterateUntilM (not . predicate) action state
  return ()

main :: IO ()
main = do
  putStrLn "WaveToy1"
  let skel = skeletonGrid (xmin, xmax) np
  let iter = 0
  let state = initGrid tini skel
  output (iter, state)
  iterateWhileM_ cond step (iter, state)
  where
    cond (iter, state) = iter < niters
    step (iter, state) = do
      let iter' = iter + 1
      let state' = rk2Grid dt rhs state
      output (iter', state')
      return (iter', state')
    rhs s = rhsGrid (bcGrid s) s

output
  :: (Floating a, Show a)
  => (Int, Grid a (Cell a)) -> IO ()
output (iter, state) = do
  guard $ not (iter == niters || iter `mod` out_every == 0)
  putStrLn $ "iteration: " ++ show iter
  putStrLn $ "  time: " ++ show (time state)
  let energy = integralGrid $ energyGrid state
  putStrLn $ "  energy: " ++ show energy
  let error = normGrid $ errorGrid state
  putStrLn $ "  L2 error: " ++ show error
