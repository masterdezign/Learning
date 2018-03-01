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

  -- * Principal components analysis
  , PCA (..)
  , pca
  , pca'
  , pcaVariance

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
  -- ** Classification
  , accuracy
  , errorRate
  , errors
  , showConfusion
  , confusion
  , Normalize (..)
  , confusion'

  -- ** Regression
  , nrmse
  ) where

import           Numeric.LinearAlgebra
import qualified Data.Vector.Storable as V
import qualified Data.Map as M
import           Data.List ( nub, sort )
import           Text.Printf ( printf )


-- | A dataset representation for supervised learning
data Dataset a b = Dataset
  { _samples :: [a]
  , _labels :: [b]
  , toList :: [(a, b)]
  }

-- | Create a `Dataset` from list of samples (first) and labels (second)
fromList :: [(a, b)] -> Dataset a b
fromList xs = let (samples', labels') = unzip xs
              in Dataset
                 { Learning.toList = xs
                 , _samples = samples'
                 , _labels = labels'
                 }

-- The snippet below computes "covariance matrix", alternative to (snd. meanCov).
-- Source: https://hackage.haskell.org/package/mltool-0.1.0.2/docs/src/MachineLearning.PCA.html
--
--     > covarianceMatrix :: Matrix Double -> Matrix Double
--     > covarianceMatrix x = ((tr x) <> x) / (fromIntegral $ rows x)

-- | Compute the covariance matrix @sigma@
-- and return its eigenvectors @u'@ and eigenvalues @s@
pca' :: [Vector Double]  -- ^ Data samples
     -> (Matrix Double, Vector Double)
pca' xs = (u', s)
  where
    -- Covariance matrix
    sigma = snd $ meanCov $ fromRows xs
    -- Eigenvectors matrix u' and eigenvalues vector s
    (u', s, _) = svd $ unSym sigma

-- | Principal components analysis tools
data PCA = PCA
  { _u :: Matrix Double
  -- ^ Compression matrix U
  , _compress :: Vector Double -> Vector Double
  -- ^ Compression function
  , _decompress :: Vector Double -> Vector Double
  -- ^ Inverse to compression function
  }

-- | Principal components analysis resulting in `PCA` tools
pca :: Int  -- ^ Number of principal components to preserve
    -> [Vector Double]  -- ^ Observations
    -> PCA
pca maxDim xs = let (u', _) = pca' xs
                in _pca maxDim u'

-- | Perform PCA using the minimal number of principal
-- components required to retain given variance
pcaVariance :: Double  -- ^ Retained variance, %
            -> [Vector Double]  -- ^ Observations
            -> PCA
pcaVariance var xs = let (u', eig) = pca' xs
                         cumul = V.drop 1 $ V.scanl' (+) 0 eig
                         var' = var / 100  -- Scale 100% -> 1.0
                         total = V.last cumul
                         isRetained = map (\v -> let retained = v / total
                                                 in retained >= var') $ V.toList cumul
                         dim = fst $ head $ filter snd $ zip [1..] isRetained
                     in _pca dim u'

_pca :: Int -> Matrix Double -> PCA
_pca maxDim u' = let u = takeColumns maxDim u'
                 in PCA
                    { _u = u
                    , _compress = flatten. (tr u <>). reshape 1
                    , _decompress = flatten. (u <>). reshape 1
                    }

-- | Classifier function that maps some measurements as matrix columns
-- and corresponding features as rows, into a categorical output.
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
-- Similar to `learnRegressor`, but instead of a `Regressor` function
-- a (already transposed) `Readout` matrix may be returned.
learn'
  :: Matrix Double  -- ^ Measurements (feature matrix)
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
  -> Vector a  -- ^ Vector of possible classes (labels)
  -> Matrix Double  -- ^ Input matrix
  -> a  -- ^ Label
winnerTakesAll readout klasses = clf
  where clf x = let klass = maxIndex $ scores readout x
                in klasses V.! klass

-- | Evaluate the network state (nonlinear response) according
-- to some `Readout` matrix. Used by classification strategies
-- such as `winnerTakesAll`.
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
classify' w kl = Classifier (winnerTakesAll w kl)
{-# SPECIALIZE classify'
  :: Matrix Double -> Vector Int -> Classifier Int
  #-}

-- | Error rate in %, an error measure for classification tasks
--
-- >>> errorRate [1,2,3,4] [1,2,3,7]
-- 25.0
errorRate :: (Eq a, Fractional err) => [a] -> [a] -> err
errorRate tgtLbls cLbls = 100 * fromIntegral errNo / fromIntegral (length tgtLbls)
  where errNo = length $ errors $ zip tgtLbls cLbls
{-# SPECIALIZE errorRate :: [Int] → [Int] → Double #-}

-- | Accuracy of classification, @100% - `errorRate`@
--
-- >>> accuracy [1,2,3,4] [1,2,3,7]
-- 75.0
accuracy :: (Eq lab, Fractional acc) => [lab] -> [lab] -> acc
accuracy tgt clf = let erate = errorRate tgt clf
                   in 100 - erate
{-# SPECIALIZE accuracy :: [Int] → [Int] → Double #-}

-- | Confusion matrix for arbitrary number of classes (not normalized)
confusion' :: (Ord lab, Eq lab)
          => [lab]
          -- ^ Target labels
          -> [lab]
          -- ^ Predicted labels
          -> M.Map (lab, lab) Int
          -- ^ Map keys: (target, predicted), values: confusion count
confusion' tgtlab lab = mp
  where
    -- Count all possible pairs of labels
    mp = foldr (M.alter f) M.empty $ zip tgtlab lab

    f Nothing = Just 1
    f (Just x) = Just (x + 1)

-- | Normalization strategies for `confusion` matrix
data Normalize = ByRow | ByColumn deriving (Show, Eq)

-- | Normalized confusion matrix for arbitrary number of classes
confusion :: (Ord lab, Eq lab)
          => Normalize
          -- ^ Normalize `ByRow` or `ByColumn`
          -> [lab]
          -- ^ Target labels
          -> [lab]
          -- ^ Predicted labels
          -> M.Map (lab, lab) Double
          -- ^ Map keys: (target, predicted), values: normalized confusion
confusion by tgtlab lab = res
  where
    allLabels = sort $ nub tgtlab
    mp = confusion' tgtlab lab
    lookup2 k' mp' = case M.lookup k' mp' of
      Just x -> x
      _ -> 0

    res = foldr (\i mp' -> let key j = if by == ByRow
                                 then (i, j)
                                 else (j, i)
                               -- Find sum
                               grp = map (\j -> let k = key j in (k, lookup2 k mp)) allLabels
                               total = fromIntegral $ sum $ map snd grp
                           -- Normalize
                           in foldr (\(k, v) mp'' -> M.insert k (fromIntegral v / total * 100) mp'') mp' grp
                ) M.empty allLabels

-- | Confusion matrix normalized by row: ASCII representation.
--
-- Note: it is assumed that target (true) labels list contains
-- all possible labels.
--
-- @
--           |  Predicted
--        ---+------------
--           | \_ \_ \_ \_ \_
--      True | \_ \_ \_ \_ \_
--           | \_ \_ \_ \_ \_
--     label | \_ \_ \_ \_ \_
--           | \_ \_ \_ \_ \_
-- @
--
-- >>> putStr $ showConfusion [1, 2, 3, 1] [1, 2, 3, 2]
--       1     2     3
-- 1   50.0  50.0   0.0
-- 2    0.0 100.0   0.0
-- 3    0.0   0.0 100.0
showConfusion :: (Ord lab, Eq lab, Show lab)
          => [lab]  -- ^ Target labels
          -> [lab]  -- ^ Predicted labels
          -> String
showConfusion tgtlab lab = unlines $ predictedLabels: "": table
  where
    allLabels = sort $ nub tgtlab
    mp = confusion ByRow tgtlab lab
    table = map (fmtRow mp) allLabels

    predictedLabels = let spc1 = replicate 2 ' '
                          spc2 = replicate 4 ' '
                      in spc1 ++ unwords (map ((spc2 ++). show) allLabels)

    -- Tabulate row
    fmtRow mp i = unwords (show i: "": line)
      where
        fmt x = let s = printf "%.1f" x
                    l = length s
                in replicate (5 - l) ' ' ++ s
        line = map (\j -> fmt $ mp M.! (i, j)) allLabels

-- | Pairs of misclassified and correct values
--
-- >>> errors $ zip ['x','y','z'] ['x','b','a']
-- [('y','b'),('z','a')]
errors :: Eq lab => [(lab, lab)] -> [(lab, lab)]
errors = filter (uncurry (/=))
{-# SPECIALIZE errors :: [(Int, Int)] -> [(Int, Int)] #-}

mean :: (V.Storable a, Fractional a) => Vector a -> a
mean xs = V.sum xs / fromIntegral (V.length xs)
{-# SPECIALISE mean :: Vector Double -> Double #-}

cov :: (V.Storable a, Fractional a) => Vector a -> Vector a -> a
cov xs ys = V.sum (V.zipWith (*) xs' ys') / fromIntegral (V.length xs')
  where
    xs' = V.map (`subtract` mean xs) xs
    ys' = V.map (`subtract` mean ys) ys
{-# SPECIALISE cov :: Vector Double -> Vector Double -> Double #-}

var :: (V.Storable a, Fractional a) => Vector a -> a
var x = cov x x
{-# SPECIALISE var :: Vector Double -> Double #-}

-- | Normalized root mean square error (NRMSE),
-- one of the most common error measures for regression tasks
nrmse :: (V.Storable a, Floating a)
      => Vector a  -- ^ Target signal
      -> Vector a  -- ^ Predicted signal
      -> a  -- ^ NRMSE
nrmse target estimated = sqrt (meanerr / targetVariance)
  where
    meanerr = mean. V.map (^2) $ V.zipWith (-) estimated target
    targetVariance = var target
{-# SPECIALIZE nrmse :: Vector Double -> Vector Double -> Double #-}
