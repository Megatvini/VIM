
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MulvariateRandomForestVarImp

<!-- badges: start -->
<!-- badges: end -->

The goal of MulvariateRandomForestVarImp package is to calculates
post-hoc variable importance measures for multivariate random forests.
These are given by split improvement for splits defined by feature j as
measured using user-defined (i.e. training or test) examples. Importance
measures can also be calculated on a per-outcome variable basis using
the change in predictions for each split. Both measures can be
optionally thresholded to include only splits that produce statistically
significant changes as measured by an F-test.

## Installation

You can install the released version of VIM from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("MulvariateRandomForestVarImp")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Megatvini/VIM")
```

## Example

This is a basic example which shows you how use the package:

``` r
library(MulvariateRandomForestVarImp)
## basic example code

X <- matrix(runif(50*5), 50, 5)
Y <- matrix(runif(50*2), 50, 2)

split_improvement_importance <- MeanSplitImprovement(X, Y)
split_improvement_importance
#> [1] 1.290106 1.108137 1.956719 1.916603 2.482978

mean_outccome_diff_importance <- MeanOutcomeDifference(X, Y)
mean_outccome_diff_importance
#>           [,1]      [,2]
#> [1,] 0.2350547 0.1496544
#> [2,] 0.2042492 0.1378401
#> [3,] 0.3373588 0.2527277
#> [4,] 0.3425937 0.1611855
#> [5,] 0.2626985 0.2430743
```
