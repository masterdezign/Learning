-- |
-- = Machine learning utilities
-- 
-- A micro library containing the most common machine learning tools.
-- Check also the mltool package https://hackage.haskell.org/package/mltool.

{-# LANGUAGE UnicodeSyntax #-}
module Learning (
  -- * Datasets
  Dataset (..)

  -- * Principal component analysis
  , PCA (..)
  , pca

  -- * Supervised learning
  , Classifier
  , learn
  , learn'
  , teacher
  , scores
  , winnerTakesAll

  -- * Evaluation
  , errors
  , errorRate
  ) where

import           Numeric.LinearAlgebra
import qualified Data.Vector.Storable as V

-- Supervised dataset
data Dataset a b = Dataset { _samples :: [a], _labels :: [b] }

-- | Computes "covariance matrix", alternative to (snd. meanCov).
-- Source: https://hackage.haskell.org/package/mltool-0.1.0.2/docs/src/MachineLearning.PCA.html
-- covarianceMatrix :: Matrix Double -> Matrix Double
-- covarianceMatrix x = ((tr x) <> x) / (fromIntegral $ rows x)

-- | Produces a compression matrix u'
pca' :: Int -> [Vector Double] -> Matrix Double
pca' maxDim xs = tr u ? [0..maxDim - 1]
  where
    xs' = fromBlocks $ map ((: []). tr. reshape 1) xs
    -- Covariance matrix Sigma
    sigma = snd $ meanCov xs'
    -- Eigenvectors matrix U
    (u, _, _) = svd $ unSym sigma

data PCA = PCA
  { _u :: Matrix Double  -- Compression matrix U
  , _compress :: Vector Double -> Matrix Double
  , _decompress :: Matrix Double -> Vector Double
  }

-- | Principal component analysis (PCA)
pca :: Int
    -> [Vector Double]
    -> PCA
pca maxDim xs = let u' = pca' maxDim xs
                    u = tr u'
                in PCA
                   { _u = u
                   , _compress = (u' <>). reshape 1
                   , _decompress = flatten. (u <>)
                   }

type Classifier a = (Matrix Double -> a)

-- | Perform supervised learning to create a linear classifier.
-- The ridge regression is run with regularization parameter mu=1e-4.
learn
  :: V.Storable a =>
     Vector a
     -> Matrix Double
     -> Matrix Double
     -> Either String (Classifier a)
learn klasses xs teacher' =
  case learn' xs teacher' of
    Just readout -> Right (classify readout klasses)
    Nothing -> Left "Couldn't learn: check `xs` matrix properties"
{-# SPECIALIZE learn
  :: Vector Int
     -> Matrix Double
     -> Matrix Double
     -> Either String (Classifier Int) #-}

-- | Create a linear readout using the ridge regression
learn'
  :: Matrix Double
     -> Matrix Double
     -> Maybe (Matrix Double)
learn' a b = case ridgeRegression 1e-4 a b of
    (Just x) -> Just (tr x)
    _ -> Nothing

-- | Teacher matrix
teacher :: Int -> Int -> Int -> Matrix Double
teacher nLabels correctIndex repeatNo = fromBlocks. map f $ [0..nLabels-1]
  where ones = konst 1.0 (1, repeatNo)
        zeros = konst 0.0 (1, repeatNo)
        f i | i == correctIndex = [ones]
            | otherwise = [zeros]

-- | Performs the supervised training that results in a linear readout.
-- See https://en.wikipedia.org/wiki/Tikhonov_regularization
ridgeRegression :: 
  Double  -- ^ Regularization constant
  -> Matrix Double
  -> Matrix Double 
  -> Maybe (Matrix Double)
ridgeRegression μ tA tB = linearSolve oA oB
  where
    oA = (tA <> tr tA) + (scalar μ * ident (rows tA))
    oB = tA <> tr tB
    _f Nothing = Nothing
    _f (Just x) = Just (tr x)

-- | Winner-takes-all classification method
winnerTakesAll
  :: V.Storable a
  => Matrix Double  -- ^ Transposed readout matrix
  -> Vector a  -- ^ Vector of possible classes
  -> Classifier a  -- ^ `Classifier`
winnerTakesAll readout klasses response = klasses V.! klass
  where klass = maxIndex $ scores readout response

-- | Evaluate the network state (nonlinear response) according
-- to some readout matrix trW.
scores :: Matrix Double -> Matrix Double -> Vector Double
scores trW response = evalScores
  where w = trW <> response
        -- Sum the elements in each row
        evalScores = w #> vector (replicate (cols w) 1.0)

classify
  :: V.Storable a
     => Matrix Double -> Vector a -> Classifier a
classify = winnerTakesAll
{-# SPECIALIZE classify
  :: Matrix Double -> Vector Int -> Classifier Int
  #-}

-- | Calculates the error rate in %
errorRate :: (Eq a, Fractional err) => [a] -> [a] -> err
errorRate tgtLbls cLbls = 100 * fromIntegral errNo / fromIntegral (length tgtLbls)
  where errNo = length $ errors $ zip tgtLbls cLbls
{-# SPECIALIZE errorRate :: [Int] → [Int] → Double #-}

-- | Returns the misclassified cases
errors :: Eq a => [(a, a)] -> [(a, a)]
errors = filter (uncurry (/=))
{-# SPECIALIZE errors :: [(Int, Int)] -> [(Int, Int)] #-}
