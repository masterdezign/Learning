module Numeric.LinearAlgebra.Extra
  ( productAB'
  , productAA'
  , productAA_
  )
  where

import           Foreign.Storable ()
import           Numeric.LinearAlgebra
import qualified Data.Vector.Storable as V


-- | Compute matrix \( \mathbf{A} \cdot \mathbf{A}^\intercal \)
--
-- >>> let a = (2 >< 3) [1..6]
-- >>> a
-- (2><3)
--  [ 1.0, 2.0, 3.0
--  , 4.0, 5.0, 6.0 ]
-- >>> productAA_ a
-- (2><2)
--  [ 14.0, 32.0
--  , 32.0, 77.0 ]
productAA_ :: Matrix Double
           -> Matrix Double
productAA_ m = productAA' (toColumns m)

productAA' :: [Vector Double]
           -> Matrix Double
productAA' vs = productAB' vs vs

-- | Compute matrix \( \mathbf{A} \cdot \mathbf{B}^\intercal \)
-- given lists of vector-columns from \( \mathbf{A} \)
-- and rows from \( \mathbf{B}^\intercal \)
-- (columns from \( \mathbf{B} \))
--
-- >>> let as = toColumns $ (2 >< 3) [1..6]
-- >>> as
-- [[1.0,4.0],[2.0,5.0],[3.0,6.0]]
-- >>> let bsTr = toRows $ (3 >< 3) [5..13]
-- [[5.0,6.0,7.0],[8.0,9.0,10.0],[11.0,12.0,13.0]]
-- >>> productAB' as bsTr
-- (2><3)
--  [  54.0,  60.0,  66.0
--  , 126.0, 141.0, 156.0 ]
productAB' :: [Vector Double]
           -> [Vector Double]
           -> Matrix Double
productAB' as bs = foldr add zero partials
  where
    zero = (_rows >< _cols) (replicate (_rows * _cols) 0)
    _rows = V.length $ head as
    _cols = V.length $ head bs
    partials = zipWith outer as bs
