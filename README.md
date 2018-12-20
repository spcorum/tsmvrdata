
<!-- README.md is generated from README.Rmd. Please edit that file -->
tsmvrextras
===========

**tsmvrextras** is the companion R package to the [tsmvr](https://github.com/spcorum/tsmvr) (Truly Sparse Multivariate Regression) package. **tsmvrextras** adds functionality by allowing the user to create synthetic datasets or load real datasets that are readily inputted into the *tsmvr* algorithm. It also contains functions to visualize and evaluate the quality of the solutions found by *tsmvr*.

Development Status
------------------

#### Builds

[![Travis build status](https://travis-ci.org/spcorum/tsmvrextras.svg?branch=master)](https://travis-ci.org/spcorum/tsmvrextras)

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/spcorum/tsmvrextras?branch=master&svg=true)](https://ci.appveyor.com/project/spcorum/tsmvrextras)

#### Test coverage

[![Coverage status](https://codecov.io/gh/spcorum/tsmvrextras/branch/master/graph/badge.svg)](https://codecov.io/github/spcorum/tsmvrextras?branch=master)

Installation
------------

You can install the development version of tsmvrextras from [github](https://github.com) with:

``` r
library(devtools)
install_github("tsmvrextras")
```

Examples
--------

Create a synthetic dataset and use *tsmvr* to perform sparse multivariate regression.

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

Evaluate the quality of the solution.

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

Visualize the iterates converging on the solution.

<img src="man/figures/README-pressure-1.png" width="100%" />

Load a real dataset and use *tsmvr* to perform sparse multivariate regression.

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

Alternatively perform k-fold cross-validation using *tsmvr* on the same dataset.

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```
