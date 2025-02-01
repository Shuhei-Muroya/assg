
# assg

<!-- badges: start -->
<!-- badges: end -->

The assg package automatically provides appropriate setting values for glmnet.
It selects the appropriate setting values for glmnet based on the given data.
Additionally, it can compare lars and glmnet in terms of computation time and choose the better package for the task.

## Installation

You can install the development version of assg from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
library(devtools)
devtools::install_github("Shuhei-Muroya/assg")
```

## Example

This is a basic example :

``` r
library(assg)

## Import example data
x_data <- get("x", envir = .myPackageEnv)
y_data <- get("y", envir = .myPackageEnv)

## Automatically select the setting values for glmnet
auto_settingvalue(x_data,T_hope=20,size = 1000)

## Automatically select the setting values and compute the lasso
auto_lasso(x_data,y_data)
```

