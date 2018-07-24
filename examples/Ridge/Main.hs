import           Learning
                 ( ridgeRegression
                 , ridgeRegression'
                 )
import           Numeric.LinearAlgebra as LA
import           Prelude hiding ( (<>) )
import           Streamly
import qualified Streamly.Prelude as S
import qualified Data.ByteString.Lazy.Char8 as LC

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

-- Parses line
parse :: LC.ByteString -> LA.Vector Double
parse s = LA.fromList (_ws s)

_ws :: LC.ByteString -> [Double]
_ws s = map (read. LC.unpack) (LC.words s)

-- Parses line and prepends constant 1
parse' :: LC.ByteString -> LA.Vector Double
parse' s = LA.fromList (1: _ws s)

getRecords :: (LC.ByteString -> LA.Vector Double) -> String -> IO [LA.Vector Double]
getRecords f path = do
  content <- LC.readFile path
  let llns = map f (LC.lines content)
  return llns

-- Simple version
mainRidgeReg1 :: IO ()
mainRidgeReg1 = do
  xs <- (addBiases. tr') <$> loadMatrix "examples/Ridge/in.txt"
  ys <- tr' <$> loadMatrix "examples/Ridge/out.txt"

  let regr1 = ridgeRegression 1e-4 xs ys

  putStrLn "Data dimensions"
  print (rows xs, cols xs)
  print (rows ys, cols ys)

  putStrLn "Ridge regression results"
  putStrLn $ show regr1

-- Streamly version
mainRidgeReg2 :: IO ()
mainRidgeReg2 = do
  xs <- S.fromList <$> getRecords parse' "examples/Ridge/in.txt"
  ys <- S.fromList <$> getRecords parse "examples/Ridge/out.txt"

  regr2 <- ridgeRegression' 1e-4 xs ys

  putStrLn $ show regr2

main :: IO ()
main = mainRidgeReg1 >> mainRidgeReg2

-- Original coefficients:
-- k0 = 7.8
-- k1 = 0.5
-- k2 = 2.4
--
-- Data generated as:
-- y = k1 * x1 + k2 * x2 + k0 + noise

-- Program output:
--
-- Data dimensions
-- (3,40)
-- (1,40)
-- Ridge regression results
-- Just (3><1)
--  [   7.794338804200687
--  , 0.49704464600013715
--  ,   2.406565742193584 ]
-- Just (3><1)
--  [   7.794338804200687
--  , 0.49704464600013715
--  ,   2.406565742193584 ]
