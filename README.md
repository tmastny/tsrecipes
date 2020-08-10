
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsrecipes

<!-- badges: start -->

<!-- badges: end -->

The goal of tsrecipes is to provide steps for the
[recipes](https://github.com/tidymodels/recipes/) package and
[tidymodels](https://www.tidymodels.org/) framework for preprocessing
time series into features for machine learning.

This package is modeled after the
[textrecipes](https://github.com/tidymodels/textrecipes): both packages
transform a sequence of data (words and time series).

In addition to the unique addition of the discrete cosine transform,
tsrecipes also provides recipe steps for transformations found in the
[feasts](https://feasts.tidyverts.org/) R package.

And features from the Python package
[tsfresh](https://github.com/blue-yonder/tsfresh), although at the
moment it contains many more transformations.

## Installation

You can install tsrecipes with:

``` r
# install.packages("devtools")
devtools::install_github("tmastny/tsrecipes")
```

## Feature Extraction

``` r
library(tsrecipes)
```

## Feature Selection

todo: probably want to never remove the ts column and have the user
`step_rm` it at the end.

textrecipes always overwrites columns, but that has the problem of
comparing original to reconstruction.

## References

Sayood, K. (2006). *Introduction to data compression*.
