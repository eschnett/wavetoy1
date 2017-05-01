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
alpha = 1/2
dt :: Double
dt = alpha * dx
niters :: Int
niters = round (fromIntegral np / alpha)
out_every :: Int
out_every = niters `div` 10

iterateWhileM_ :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m ()
iterateWhileM_ predicate action state =
  do _ <- iterateUntilM (not . predicate) action state
     return ()

main :: IO ()
main =
  do putStrLn "WaveToy1"
     let skel = skeletonGrid (xmin, xmax) np
     let state = initGrid tini skel
     output state
     iterateWhileM_ cond step state
  where cond state = iter state < niters
        step state = do let state' = inc $ rk2Grid dt rhs state
                        output state'
                        return state'
        rhs s = rhsGrid (bcGrid s) s
        inc s = s { iter = iter s + 1 }

output :: (Floating a, Show a) => Grid a (Cell a) -> IO ()
output state =
  do guard $ not (iter state == niters || iter state `mod` out_every == 0)
     putStrLn $ "iteration: " ++ show (iter state)
     putStrLn $ "  time: " ++ show (time state)
     let energy = integralGrid $ energyGrid state
     putStrLn $ "  energy: " ++ show energy
     let error = normGrid $ errorGrid state
     putStrLn $ "  L2 error: " ++ show error
