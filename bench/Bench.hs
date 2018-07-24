import           Control.DeepSeq ( force )
import           Criterion.Main
import qualified Learning as L
import           Numeric.LinearAlgebra
import           Prelude hiding ( (<>) )
import           Streamly
import qualified Streamly.Prelude as S

runBench1 :: (Matrix Double, Matrix Double) -> Maybe Double
runBench1 (xs, ys) = fmap sumElements $ L.ridgeRegression 1e-4 xs ys

runBench2 :: IO (SerialT IO (Vector Double), SerialT IO (Vector Double)) -> IO (Maybe Double)
runBench2 arg = do
  (xs', ys') <- arg
  w <- L.ridgeRegression' 1e-4 xs' ys'
  return (fmap sumElements w)

main :: IO ()
main = defaultMain [
    bench "ridgeRegression" $ whnf runBench1 (xs, ys)
    , bench "ridgeRegression' (streamly)" $ nfIO $ runBench2 (pure (xs', ys'))
  ]
  where
    seed1 = 5050
    seed2 = 20100
    dim1 = 1000
    dimX = 50
    dimY = 1
    xs = reshape dim1 (randomVector seed1 Uniform (dim1 * dimX))
    ys = reshape dim1 (randomVector seed1 Uniform (dim1 * dimY))
    xs' = S.fromList $ toColumns xs
    ys' = S.fromList $ toColumns ys

-- Benchmark was run on Dell Precision T3610
--
-- benchmarking ridgeRegression
-- time                 885.7 μs   (872.3 μs .. 905.0 μs)
--                      0.997 R²   (0.995 R² .. 0.999 R²)
-- mean                 907.6 μs   (898.8 μs .. 921.3 μs)
-- std dev              37.20 μs   (27.35 μs .. 52.90 μs)
-- variance introduced by outliers: 31% (moderately inflated)

-- benchmarking ridgeRegression' (streamly)
-- time                 10.70 ms   (10.52 ms .. 10.89 ms)
--                      0.997 R²   (0.994 R² .. 1.000 R²)
-- mean                 10.67 ms   (10.57 ms .. 10.88 ms)
-- std dev              357.7 μs   (172.4 μs .. 632.9 μs)
-- variance introduced by outliers: 13% (moderately inflated)
