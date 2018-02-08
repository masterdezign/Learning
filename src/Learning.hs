-- |
-- = Machine learning utilities
--
-- A micro library containing the most common machine learning tools.
-- Check also the mltool package https://hackage.haskell.org/package/mltool.

{-# LANGUAGE UnicodeSyntax #-}
module Learning (
  -- * Datasets
  Dataset (..)
  , Learning.fromList

  -- * Principal component analysis
  , PCA (..)
  , pca

  -- * Supervised learning
  , Teacher
  , teacher
  , Classifier (..)
  , Regressor (..)
  , Readout
  , learnClassifier
  , learnRegressor
  , learn'
  , scores
  , winnerTakesAll

  -- * Evaluation
  , errors
  , errorRate
  , nrmse
  ) where

import           Numeric.LinearAlgebra
import qualified Data.Vector.Storable as V

-- | A dataset representation for supervised learning
data Dataset a b = Dataset
  { _samples :: [a]
  , _labels :: [b]
  , _raw :: [(a, b)]
  }

-- | Create a `Dataset` from list of samples (first) and labels (second)
fromList :: [(a, b)] -> Dataset a b
fromList xs = let (samples', labels') = unzip xs
              in Dataset
                 { _raw = xs
                 , _samples = samples'
                 , _labels = labels'
                 }

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

-- | Principal components analysis tools
data PCA = PCA
  { _u :: Matrix Double
  -- ^ Compression matrix U
  , _compress :: Vector Double -> Matrix Double
  -- ^ Compression function
  , _decompress :: Matrix Double -> Vector Double
  -- ^ Inverse to compression function
  }

-- | Principal component analysis (PCA)
pca :: Int  -- ^ Number of principal components to preserve
    -> [Vector Double]  -- ^ Analyzed data samples
    -> PCA
pca maxDim xs = let u' = pca' maxDim xs
                    u = tr u'
                in PCA
                   { _u = u
                   , _compress = (u' <>). reshape 1
                   , _decompress = flatten. (u <>)
                   }

-- | Classifier function that maps some network state with measurements as matrix columns
-- and features as rows, into a categorical output.
newtype Classifier a = Classifier { classify :: Matrix Double -> a }

-- | Regressor function that maps some feature matrix
-- into a continuous multidimensional output. The feature matrix is expected
-- to have columns corresponding to measurements (data points) and rows, features.
newtype Regressor = Regressor { predict :: Matrix Double -> Matrix Double }

-- | Linear readout (matrix)
type Readout = Matrix Double

-- | Teacher matrix
--
--     > 0 0 0 0 0
--     > 0 0 0 0 0
--     > 1 1 1 1 1 <- Desired class index is 2
--     > 0 0 0 0 0 <- Number of classes is 4
--     >         ^
--     >   5 repetitions
type Teacher = Matrix Double

-- | Perform supervised learning (ridge regression) and create
-- a linear `Classifier` function.
-- The regression is run with regularization parameter μ = 1e-4.
learnClassifier
  :: (V.Storable a, Eq a) =>
     Vector a
     -- ^ All possible outcomes (classes) list
     -> Matrix Double
     -- ^ Network state (nonlinear response) where each matrix column corresponds to a measurement (data point)
     -- and each row corresponds to a feature
     -> Matrix Double
     -- ^ Horizontally concatenated `Teacher` matrices where each row corresponds to a desired class
     -> Either String (Classifier a)
learnClassifier klasses xs teacher' =
  case learn' xs teacher' of
    Just readout -> Right (classify' readout klasses)
    Nothing -> Left "Couldn't learn: check `xs` matrix properties"
{-# SPECIALIZE learnClassifier
  :: Vector Int
     -> Matrix Double
     -> Matrix Double
     -> Either String (Classifier Int) #-}

-- | Perform supervised learning (ridge regression) and create
-- a linear `Regressor` function.
learnRegressor
  :: Matrix Double
  -- ^ Feature matrix with data points (measurements) as colums and features as rows
  -> Matrix Double
  -- ^ Desired outputs matrix corresponding to data point columns.
  -- In case of scalar (one-dimensional) prediction output, it should be a single row matrix.
  -> Either String Regressor
learnRegressor xs target =
  case learn' xs target of
    Just readout -> let rgr = Regressor (readout <>)
                    in Right rgr
    Nothing -> Left "Couldn't learn: check `xs` matrix properties"

-- | Create a linear `Readout` using the ridge regression.
-- Similar to @learn@, but instead of a `Classifier` function
-- a (already transposed) `Readout` matrix may be returned.
learn'
  :: Matrix Double  -- ^ Network state (nonlinear response)
  -> Matrix Double  -- ^ Horizontally concatenated `Teacher` matrices
  -> Maybe Readout
learn' a b = case ridgeRegression 1e-4 a b of
    (Just x) -> Just (tr x)
    _ -> Nothing

-- | Create a binary `Teacher` matrix with ones row corresponding to
-- the desired class index
teacher
  :: Int  -- ^ Number of classes (labels)
  -> Int  -- ^ Desired class index (starting from zero)
  -> Int  -- ^ Number of repeated columns in teacher matrix
  -> Teacher
teacher nLabels correctIndex repeatNo = fromBlocks. map f $ [0..nLabels-1]
  where ones = konst 1.0 (1, repeatNo)
        zeros = konst 0.0 (1, repeatNo)
        f i | i == correctIndex = [ones]
            | otherwise = [zeros]

-- | Performs a supervised training that results in a linear readout.
-- See https://en.wikipedia.org/wiki/Tikhonov_regularization
ridgeRegression ::
  Double  -- ^ Regularization constant
  -> Matrix Double
  -> Matrix Double
  -> Maybe Readout
ridgeRegression μ tA tB = linearSolve oA oB
  where
    oA = (tA <> tr tA) + (scalar μ * ident (rows tA))
    oB = tA <> tr tB
    _f Nothing = Nothing
    _f (Just x) = Just (tr x)

-- | Winner-takes-all classification method
winnerTakesAll
  :: (V.Storable a, Eq a)
  => Readout  -- ^ `Readout` matrix
  -> Vector a  -- ^ Vector of possible classes
  -> Classifier a  -- ^ `Classifier`
winnerTakesAll readout klasses = Classifier clf
  where clf x = let klass = maxIndex $ scores readout x
                in klasses V.! klass

-- | Evaluate the network state (nonlinear response) according
-- to some `Readout` matrix.
scores
  :: Readout  -- ^ `Readout` matrix
  -> Matrix Double  -- ^ Network state
  -> Vector Double
scores trW response = evalScores
  where w = trW <> response
        -- Sum the elements in each row
        evalScores = w #> vector (replicate (cols w) 1.0)

classify'
  :: (V.Storable a, Eq a)
     => Matrix Double -> Vector a -> Classifier a
classify' = winnerTakesAll
{-# SPECIALIZE classify'
  :: Matrix Double -> Vector Int -> Classifier Int
  #-}

-- | Error rate in %
errorRate :: (Eq a, Fractional err) => [a] -> [a] -> err
errorRate tgtLbls cLbls = 100 * fromIntegral errNo / fromIntegral (length tgtLbls)
  where errNo = length $ errors $ zip tgtLbls cLbls
{-# SPECIALIZE errorRate :: [Int] → [Int] → Double #-}

-- | Misclassified cases
errors :: Eq a => [(a, a)] -> [(a, a)]
errors = filter (uncurry (/=))
{-# SPECIALIZE errors :: [(Int, Int)] -> [(Int, Int)] #-}

mean :: (V.Storable a, Fractional a) => Vector a -> a
mean xs = V.sum xs / fromIntegral (V.length xs)
{-# SPECIALISE mean :: Vector Double -> Double #-}

cov :: (V.Storable a, Fractional a) => Vector a -> Vector a -> a
cov xs ys = V.sum (V.zipWith (*) xs' ys') / fromIntegral (V.length xs')
  where
    xs' = V.map (`subtract` (mean xs)) xs
    ys' = V.map (`subtract` (mean ys)) ys
{-# SPECIALISE cov :: Vector Double -> Vector Double -> Double #-}

var :: (V.Storable a, Fractional a) => Vector a -> a
var x = cov x x
{-# SPECIALISE var :: Vector Double -> Double #-}

-- | Normalized root mean square error (NRMSE),
-- one of the most common error measures for series prediction
nrmse :: (V.Storable a, Floating a)
      => Vector a  -- ^ Target signal
      -> Vector a  -- ^ Predicted signal
      -> a  -- ^ NRMSE
nrmse target estimated = sqrt (meanerr / targetVariance)
  where
    meanerr = mean. V.map (^2) $ V.zipWith (-) estimated target
    targetVariance = var target
{-# SPECIALIZE nrmse :: Vector Double -> Vector Double -> Double #-}
