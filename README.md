# Learning

A Haskell micro library containing the most common machine learning tools.

The name of the package can be interpreted in two ways:

1. Either as "Learning" in "Machine Learning".
2. Or "Learning" meaning that [examples](https://github.com/masterdezign/Learning/tree/master/app)
are written in [literate style](https://en.wikipedia.org/wiki/Literate_programming)
and can be used to discover machine learning techniques.


## Features

* Supervised learning
  * Ridge regression
  * Linear classifier
* Evaluation metrics
* Principal components analysis


## Getting Started

Use [Stack](http://haskellstack.org)

     $ git clone https://github.com/masterdezign/Learning.git && cd Learning
     $ stack build --install-ghc

### Demo 1: principal components analysis (PCA)

Launch the [PCA demo](https://github.com/masterdezign/Learning/blob/master/app/MainPCA.lhs)

     $ stack exec learning-pca

### Demo 2: advanced PCA

Launch the advanced [PCA demo](https://github.com/masterdezign/Learning/blob/master/app/MainPCA2.lhs)

     $ stack exec learning-pca-advanced

### What's next?

Check the [documentation](https://hackage.haskell.org/package/Learning/docs/Learning.html)
or [open an issue](https://github.com/masterdezign/Learning/issues).
