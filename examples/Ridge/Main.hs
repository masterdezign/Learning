import           Learning
                 ( ridgeRegression
                 , ridgeRegression'
                 )
import           Numeric.LinearAlgebra as LA
import           Prelude hiding ( (<>) )

-- | Prepend a row of ones
--
-- >>> addBiases $ (2><3) [20..26]
-- (3><3)
--  [  1.0,  1.0,  1.0
--  , 20.0, 21.0, 22.0
--  , 23.0, 24.0, 25.0 ]
addBiases :: LA.Matrix Double -> LA.Matrix Double
addBiases m = let no = LA.cols m
                  m' = LA.konst 1.0 (1, no)
              in m' LA.=== m

main :: IO ()
main = do
  xs <- addBiases <$> loadMatrix "examples/Ridge/in.txt"
  ys <- tr' <$> loadMatrix "examples/Ridge/out.txt"

  let regr1 = ridgeRegression 1e-4 xs ys
  let regr2 = ridgeRegression' 1e-4 (toColumns xs) (toColumns ys)

  putStrLn "Data dimensions"
  print (rows xs, cols xs)
  print (rows ys, cols ys)

  putStrLn "Ridge regression results"
  putStrLn $ show regr1
  putStrLn $ show regr2

-- Original coefficients:
-- k0 = 7.8
-- k1 = 0.5
-- k2 = 2.4

-- Data generated as:
-- y = k1 * x1 + k2 * x2 + k0 + noise

-- Data dimensions
-- (3,40)
-- (1,40)
-- Ridge regression results
-- Just (3><1)
--  [  7.794338804200684
--  , 0.4970446460001376
--  , 2.4065657421935858 ]
-- Just (3><1)
--  [   7.794338804200684
--  , 0.49704464600013837
--  ,  2.4065657421935844 ]

