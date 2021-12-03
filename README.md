
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MulvariateRandomForestVarImp

<!-- badges: start -->
<!-- badges: end -->

The goal of MulvariateRandomForestVarImp package is to calculate
post-hoc variable importance measures for multivariate random forests.
These are given by split improvement for splits defined by feature j as
measured using user-defined (ie training or test) examples. Importances
can also be calculated on a per-outcome variable basis using the change
in predictions for each split. Both measures can be optionally
threshholded to include only splits that produce statistically
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
#> [1] 1.4039594 0.9331684 1.2427598 1.8632999 3.1798021

mean_outccome_diff_importance <- MeanOutcomeDifference(X, Y)
mean_outccome_diff_importance
#>           [,1]      [,2]
#> [1,] 0.1834112 0.1143826
#> [2,] 0.1972713 0.1633333
#> [3,] 0.2080036 0.1975544
#> [4,] 0.1381196 0.1182340
#> [5,] 0.1995608 0.1019632
```
