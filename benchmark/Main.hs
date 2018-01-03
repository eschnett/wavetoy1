-- You can benchmark your code quickly and effectively with Criterion.
-- See its website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import WaveToy1

default (Int)

main :: IO ()
main =
    defaultMain
        $ [ bgroup
                "Grid"
                [ bench "init" $ whnf
                    ( \np ->
                        let skel   = skeletonGrid (0, 1) np :: Grid Double ()
                            coords = coordGrid skel
                            g      = initGrid 0 coords
                        in  integralGrid $ energyGrid g
                    )
                    gridSize
                , bench "rhs" $ whnf
                    ( \np ->
                        let skel   = skeletonGrid (0, 1) np :: Grid Double ()
                            coords = coordGrid skel
                            g      = initGrid 0 coords
                            r      = bcGrid (rhsGrid g)
                        in  integralGrid $ energyGrid r
                    )
                    gridSize
                , bench "rk2" $ whnf
                    ( \np ->
                        let skel   = skeletonGrid (0, 1) np :: Grid Double ()
                            coords = coordGrid skel
                            g      = initGrid 0 coords
                            rhs x = bcGrid (rhsGrid x)
                            step = rk2Grid smallStep rhs
                            g'   = iterate step g !! numSteps
                        in  integralGrid $ energyGrid g'
                    )
                    gridSize
                ]
          ]

gridSize :: Int
gridSize = 1000

numSteps :: Int
numSteps = 10

smallStep :: Double
smallStep = 1.0e-8
