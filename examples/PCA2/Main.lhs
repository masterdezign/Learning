Advanced Principal Components Analysis (PCA) demo
-------------------------------------------------


The tutorial is a continuation of ./MainPCA.lhs

Previously, we were able to quickly determine that the food ration
in Northern Ireland is somewhat different comparing to the other three
countries. We have projected data from 17 dimensions into
one dimension. However, we could also make a projection into
two or more dimensions. So how do we determine which dimensionality
reduction does preserve the most of the information? How do we make
sure that only redundant information was removed?

For that purpose, we will calculate retained variance [1]
depending on the number of the principal components.

[1] http://www.dsc.ufcg.edu.br/~hmg/disciplinas/posgraduacao/rn-copin-2014.3/material/SignalProcPCA.pdf

Let's start with the imports and data definition.

> import           Learning ( pca' )
> import qualified Numeric.LinearAlgebra as LA
> import           Data.List ( scanl' )
> import           Text.Printf ( printf )

> england = [375, 57, 245, 1472, 105, 54, 193, 147, 1102,
>            720, 253, 685, 488, 198, 360, 1374, 156]

> northernIreland = [135, 47, 267, 1494, 66, 41, 209, 93, 674,
>                    1033, 143, 586, 355, 187, 334, 1506, 139]

> scotland = [458, 53, 242, 1462, 103, 62, 184, 122, 957,
>             566, 171, 750, 418, 220, 337, 1572, 147]

> wales = [475, 73, 227, 1582, 103, 64, 235, 160, 1137,
>          874, 265, 803, 570, 203, 365, 1256, 175]

> countries = map LA.fromList [england, northernIreland, scotland, wales]

In order to compute the eigenvectors u and eigenvalues eig of
a covariance matrix, we use function pca'. In fact, pca' was
already called under the hood in the previous tutorial.

> (u, eig) = pca' countries

Sums of the first N eigenvalues:

> cumul = drop 1 $ scanl' (+) 0 $ LA.toList eig

Sum of all eigenvalues:

> total = last cumul

> main = mapM_ (\(i, s) ->
>                 let retained = s / total * 100 :: Double
>                     msg = "%d principal component(s): Retained variance %.1f%%"
>                 in putStrLn $ printf msg i retained)
>              $ zip [1::Int ..] cumul

     1 principal component(s): Retained variance 67.4%
     2 principal component(s): Retained variance 96.5%
     3 principal component(s): Retained variance 100.0%
     4 principal component(s): Retained variance 100.0%

     ...

     17 principal component(s): Retained variance 100.0%

From this data we can conclude that the first two principal components
contain 96.5% of information. Therefore, we will loose 3.5% of information
after projecting into two orthogonal axes in the transformed coordinate system
obtained after PCA.

Hint: to compute compression (dimensionality reduction) and
decompression functions for specified variance to retain, use
(_compress. pcaVariance) and (_decompress. pcaVariance) functions.
