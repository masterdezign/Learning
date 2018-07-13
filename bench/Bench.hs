import           Control.DeepSeq ( force )
import           Criterion.Main
import           Numeric.LinearAlgebra
import           Prelude hiding ( (<>) )
import qualified Learning as L

runBench1 :: (Matrix Double, Matrix Double) -> Maybe Double
runBench1 (xs, ys) = fmap sumElements $ L.ridgeRegression 1e-4 xs ys

runBench2 :: ([Vector Double], [Vector Double]) -> Maybe Double
runBench2 (xs', ys') = fmap sumElements $ L.ridgeRegression' 1e-4 xs' ys'

main :: IO ()
main = defaultMain [
    bench "ridgeRegression" $ whnf runBench1 (xs, ys)
    , bench "ridgeRegression'" $ whnf runBench2 (xs', ys')
  ]
  where
    seed1 = 5050
    seed2 = 20100
    dim1 = 1000
    dimX = 50
    dimY = 1
    xs = force $ reshape dim1 (randomVector seed1 Uniform (dim1 * dimX))
    ys = force $ reshape dim1 (randomVector seed1 Uniform (dim1 * dimY))
    xs' = force $ toColumns xs
    ys' = force $ toColumns ys

-- benchmarking ridgeRegression
-- time                 348.1 μs   (347.5 μs .. 348.6 μs)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 346.3 μs   (345.4 μs .. 347.1 μs)
-- std dev              3.113 μs   (2.477 μs .. 4.121 μs)
--
-- benchmarking ridgeRegression'
-- time                 5.882 ms   (5.819 ms .. 5.945 ms)
--                      0.999 R²   (0.998 R² .. 0.999 R²)
-- mean                 5.878 ms   (5.826 ms .. 5.941 ms)
-- std dev              177.8 μs   (135.6 μs .. 264.6 μs)
-- variance introduced by outliers: 12% (moderately inflated)
