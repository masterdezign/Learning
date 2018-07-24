module Numeric.LinearAlgebra.Extra
  ( productAB'
  , productAA'
  )
  where

import           Data.Maybe
import qualified Data.Vector.Storable as V
import           Numeric.LinearAlgebra
import           Streamly
import qualified Streamly.Prelude as S

-- | Compute matrix \( \mathbf{A} \cdot \mathbf{B}^\intercal \)
-- given streams of vector-columns from \( \mathbf{A} \)
-- and rows from \( \mathbf{B}^\intercal \)
-- (columns from \( \mathbf{B} \)).
--
-- Returns Nothing if empty streams.
--
-- >>> import Numeric.LinearAlgebra
-- >>> import qualified Streamly.Prelude as S
-- >>> let as = toColumns $ (2 >< 3) [1..6]
-- [[1.0,4.0],[2.0,5.0],[3.0,6.0]]
-- >>> let bsTr = toRows $ (3 >< 3) [5..13]
-- [[5.0,6.0,7.0],[8.0,9.0,10.0],[11.0,12.0,13.0]]
-- >>> productAB' (S.fromList as) (S.fromList bsTr)
-- Just (2><3)
--  [  54.0,  60.0,  66.0
--  , 126.0, 141.0, 156.0 ]
productAB' :: Monad m
           => SerialT m (Vector Double)
           -- ^ Stream of vectors from matrix \( A \)
           -> SerialT m (Vector Double)
           -- ^ Stream of vectors from matrix \( B \)
           -> m (Maybe (Matrix Double))
productAB' as bs = do
  -- FIXME: avoid using fromJust
  let lenV = V.length. fromJust
  _rows <- lenV <$> (S.head as)
  _cols <- lenV <$> (S.head bs)
  let zero = (_rows >< _cols) (repeat 0.0)
      partials = S.zipWith outer as bs
  result <- S.foldl' add zero partials
  return (Just result)

-- | Compute matrix \( \mathbf{A} \cdot \mathbf{A}^\intercal \)
productAA' :: Monad m
           => SerialT m (Vector Double)
           -- ^ Stream of vectors from matrix \( A \)
           -> m (Maybe (Matrix Double))
productAA' vs = productAB' vs vs
