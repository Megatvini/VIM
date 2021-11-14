
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MulvariateRandomForestVarImp

<!-- badges: start -->
<!-- badges: end -->

The goal of VIM is to …

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

This is a basic example which shows you how to solve a common problem:

``` r
library(MulvariateRandomForestVarImp)
## basic example code

X <- matrix(runif(50*5), 50, 5)
Y <- matrix(runif(50*2), 50, 2)

variable_importace <- MeanSplitImprovement(X, Y)

variable_importace
#> [1] 3.5729038 0.7747768 1.3095010 2.0873875 1.0315281
```
