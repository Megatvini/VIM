
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MulvariateRandomForestVarImp

<!-- badges: start -->

[![R-CMD-check](https://github.com/Megatvini/VIM/workflows/R-CMD-check/badge.svg)](https://github.com/Megatvini/VIM/actions)
<!-- badges: end -->

The goal of MulvariateRandomForestVarImp package is to calculate
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
set.seed(49)

X <- matrix(runif(50*5), 50, 5)
Y <- matrix(runif(50*2), 50, 2)

split_improvement_importance <- MeanSplitImprovement(X, Y)
split_improvement_importance
#> [1] 0.8066173 2.8909635 3.4591123 0.6227943 0.5138745

mean_outccome_diff_importance <- MeanOutcomeDifference(X, Y)
mean_outccome_diff_importance
#>           [,1]      [,2]
#> [1,] 0.2458139 0.3182474
#> [2,] 0.2712269 0.2915053
#> [3,] 0.2125802 0.2023291
#> [4,] 0.2819759 0.2519035
#> [5,] 0.1238451 0.1958629
```
