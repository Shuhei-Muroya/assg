
# assg

<!-- badges: start -->
<!-- badges: end -->

The goal of assg is to ...

## Installation

You can install the development version of assg from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("Shuhei-Muroya/assg")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(assg)

## Import example data
x_data <- get("x", envir = .myPackageEnv)
y_data <- get("y", envir = .myPackageEnv)

## Automatically select the setting values for glmnet
auto_settingvalue(x_data,T_hope=20,size = 1000)

## Automatically select the setting values and compute the lasso
auto_lasso(y_data,x_data)
```

