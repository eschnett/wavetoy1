{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Control.Monad.Loops

import WaveToy1

-- Parameters
tini = 0::Double
xmin = 0::Double
xmax = 1::Double
np = 20::Int
dx = (xmax - xmin) / fromIntegral np :: Double
alpha = 1/2 :: Double
dt = alpha * dx :: Double
niters = round (fromIntegral np / alpha) :: Int
out_every = niters `div` 10 :: Int

rhs s = rhsGrid (bcGrid s) s
inc g = g { iter = iter g + 1 }
step s = inc $ rk2Grid dt rhs s

iterateUntilM_ predicate action state =
  do _ <- iterateUntilM predicate action state
     return ()

main :: IO ()
main = do putStrLn "WaveToy1"
          let skel = skeletonGrid (xmin, xmax) np
          let state = initGrid tini skel
          output state
          iterateUntilM_
            (\state -> iter state >= niters)
            (\state -> do let state' = step state
                          output state'
                          return state')
            state

output :: (Floating a, Show a) => Grid a (Cell a) -> IO ()
output state =
  if iter state == niters || iter state `mod` out_every == 0
  then do putStrLn $ "iteration: " ++ show (iter state)
          putStrLn $ "  time: " ++ show (time state)
          let energy = integralGrid $ energyGrid state
          putStrLn $ "  energy: " ++ show energy
          let error = normGrid $ errorGrid state
          putStrLn $ "  L2 error: " ++ show error
  else do return ()
