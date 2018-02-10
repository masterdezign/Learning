Principal Components Analysis (PCA) demo
----------------------------------------


The tutorial is based on http://setosa.io/ev/principal-component-analysis/

Suppose, we study nutrition habits of the citizens of four countries.
Here, we provide the food consumption data among those countries.

> import           Learning
> import qualified Numeric.LinearAlgebra as LA

> england = [375, 57, 245, 1472, 105, 54, 193, 147, 1102,
>            720, 253, 685, 488, 198, 360, 1374, 156]

> northernIreland = [135, 47, 267, 1494, 66, 41, 209, 93, 674,
>                    1033, 143, 586, 355, 187, 334, 1506, 139]

> scotland = [458, 53, 242, 1462, 103, 62, 184, 122, 957,
>             566, 171, 750, 418, 220, 337, 1572, 147]

> wales = [475, 73, 227, 1582, 103, 64, 235, 160, 1137,
>          874, 265, 803, 570, 203, 365, 1256, 175]

We want to know how differ the countries based on those data.
For that purpose, we would like to reduce the redundant information
or, in other words, perform PCA.

We create a single list of feature vectors (each country) used later
for the analysis.

> countries = map LA.fromList [england, northernIreland, scotland, wales]

We perform PCA, i.e. calculate the compression (dimensionality reduction)
function `compress`. The `pca` function is given
`principalComponents` parameter. Here it's 1, that means that
`countries` vectors of 17 features will be reduced into scalars (1D vectors).

> compress = let principalComponents = 1
>                pca1 = pca principalComponents countries
>            in _compress pca1

Output the resulting scalar values for each country

> main = mapM_ (print. compress) countries

Here is a summary:

     England            -702.9850482521952
     Northern Ireland   -80.6002572540017
     Scotland           -649.8612350689822
     Wales              -798.521043705295

Wales
 \/  England                       Northern Ireland
      \/                                   \/
..o....o..o.................................o
          /\
        Scotland

Now, we can clearly see that there exists a difference between
Northern Ireland and the rest of the countries.
