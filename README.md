
# correlation <img src='man/figures/logo.png' align="right" height="139" />

[![Build
Status](https://travis-ci.org/easystats/correlation.svg?branch=master)](https://travis-ci.org/easystats/correlation)
[![codecov](https://codecov.io/gh/easystats/correlation/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/correlation)
[![HitCount](http://hits.dwyl.io/easystats/correlation.svg)](http://hits.dwyl.io/easystats/correlation)
[![Documentation](https://img.shields.io/badge/documentation-correlation-orange.svg?colorB=E91E63)](https://easystats.github.io/correlation/)

`correlation` is a lightweight package helping with correlation
analysis.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/correlation")
```

``` r
library("correlation")
```

## Documentation

[![Documentation](https://img.shields.io/badge/documentation-bayestestR-orange.svg?colorB=E91E63)](https://easystats.github.io/correlation/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-correlation-orange.svg?colorB=2196F3)](https://easystats.github.io/correlation/reference/index.html)

Click on the buttons above to access the package
[**documentation**](https://easystats.github.io/correlation/) and the
[**easystats blog**](https://easystats.github.io/blog/posts/), and
check-out these vignettes:

  - No vignettes yet :(

# Features

The main function is `correlation()`, which builds on top of
`cor_test()` and comes with a number of possible options.

## Correlation details and matrix

``` r
cor <- correlation(iris)
cor
## Parameter1   |   Parameter2 |     r |     t |  df |      p |         95% CI |  Method
## -------------------------------------------------------------------------------------
## Sepal.Length |  Sepal.Width | -0.12 | -1.44 | 148 | > .1   | [-0.27,  0.04] | Pearson
## Sepal.Length | Petal.Length |  0.87 | 21.65 | 148 | < .001 | [ 0.83,  0.91] | Pearson
## Sepal.Length |  Petal.Width |  0.82 | 17.30 | 148 | < .001 | [ 0.76,  0.86] | Pearson
## Sepal.Width  | Petal.Length | -0.43 | -5.77 | 148 | < .001 | [-0.55, -0.29] | Pearson
## Sepal.Width  |  Petal.Width | -0.37 | -4.79 | 148 | < .001 | [-0.50, -0.22] | Pearson
## Petal.Length |  Petal.Width |  0.96 | 43.39 | 148 | < .001 | [ 0.95,  0.97] | Pearson
```

The output is not a square matrix, but a **(tidy) dataframe with all
correlations tests per row**. One can also obtain a **matrix** using:

``` r
summary(cor)
## Parameter    | Petal.Width | Petal.Length | Sepal.Width
## -------------------------------------------------------
## Sepal.Length |     0.82*** |      0.87*** |       -0.12
## Sepal.Width  |    -0.37*** |     -0.43*** |            
## Petal.Length |     0.96*** |              |
```

Note that one can also obtain the full, **square** and redundant matrix
using:

``` r
as.table(cor)
## Parameter    | Sepal.Length | Sepal.Width | Petal.Length | Petal.Width
## ----------------------------------------------------------------------
## Sepal.Length |      1.00*** |       -0.12 |      0.87*** |     0.82***
## Sepal.Width  |        -0.12 |     1.00*** |     -0.43*** |    -0.37***
## Petal.Length |      0.87*** |    -0.43*** |      1.00*** |     0.96***
## Petal.Width  |      0.82*** |    -0.37*** |      0.96*** |     1.00***
```

## Grouped dataframes

The function also supports **stratified correlations**, all within the
*tidyverse* workflow\!

``` r
library(dplyr)

iris %>% 
  select(Species, starts_with("Sepal"), Petal.Width) %>% 
  group_by(Species) %>% 
  correlation()
## Group      |   Parameter1 |  Parameter2 |    r |    t | df |      p |         95% CI |  Method
## ----------------------------------------------------------------------------------------------
## setosa     | Sepal.Length | Sepal.Width | 0.74 | 7.68 | 48 | < .001 | [ 0.59,  0.85] | Pearson
## setosa     | Sepal.Length | Petal.Width | 0.28 | 2.01 | 48 | > .1   | [ 0.00,  0.52] | Pearson
## setosa     |  Sepal.Width | Petal.Width | 0.23 | 1.66 | 48 | > .1   | [-0.05,  0.48] | Pearson
## versicolor | Sepal.Length | Sepal.Width | 0.53 | 4.28 | 48 | < .001 | [ 0.29,  0.70] | Pearson
## versicolor | Sepal.Length | Petal.Width | 0.55 | 4.52 | 48 | < .001 | [ 0.32,  0.72] | Pearson
## versicolor |  Sepal.Width | Petal.Width | 0.66 | 6.15 | 48 | < .001 | [ 0.47,  0.80] | Pearson
## virginica  | Sepal.Length | Sepal.Width | 0.46 | 3.56 | 48 | < .01  | [ 0.20,  0.65] | Pearson
## virginica  | Sepal.Length | Petal.Width | 0.28 | 2.03 | 48 | < .05  | [ 0.00,  0.52] | Pearson
## virginica  |  Sepal.Width | Petal.Width | 0.54 | 4.42 | 48 | < .001 | [ 0.31,  0.71] | Pearson
```

## Bayesian Correlations

It is very easy to switch to a **Bayesian framework**.

``` r
correlation(iris, bayesian=TRUE)
## Parameter1   |   Parameter2 |   rho |         89% CI |     pd | % in ROPE |    BF |              Prior
## ------------------------------------------------------------------------------------------------------
## Sepal.Length |  Sepal.Width | -0.11 | [-0.23,  0.02] | 92.58% |    43.60% |  0.51 | Cauchy (0 +- 0.33)
## Sepal.Length | Petal.Length |  0.86 | [ 0.83,  0.90] |   100% |        0% | > 999 | Cauchy (0 +- 0.33)
## Sepal.Length |  Petal.Width |  0.81 | [ 0.76,  0.85] |   100% |        0% | > 999 | Cauchy (0 +- 0.33)
## Sepal.Width  | Petal.Length | -0.42 | [-0.52, -0.30] |   100% |        0% | > 999 | Cauchy (0 +- 0.33)
## Sepal.Width  |  Petal.Width | -0.35 | [-0.47, -0.24] |   100% |     0.05% | > 999 | Cauchy (0 +- 0.33)
## Petal.Length |  Petal.Width |  0.96 | [ 0.95,  0.97] |   100% |        0% | > 999 | Cauchy (0 +- 0.33)
```

## Tetrachoric, Polychoric, Biserial, Biweight…

The `correlation` package also supports different types of methods,
which can deal with correlations **between factors**\!

``` r
correlation(iris, include_factors = TRUE, method = "auto")
## Parameter1         |         Parameter2 |     r |      t |  df |      p |         95% CI |      Method
## ------------------------------------------------------------------------------------------------------
## Sepal.Length       |        Sepal.Width | -0.12 |  -1.44 | 148 | > .1   | [-0.27,  0.04] |     Pearson
## Sepal.Length       |       Petal.Length |  0.87 |  21.65 | 148 | < .001 | [ 0.83,  0.91] |     Pearson
## Sepal.Length       |        Petal.Width |  0.82 |  17.30 | 148 | < .001 | [ 0.76,  0.86] |     Pearson
## Sepal.Length       |     Species.setosa | -0.93 | -29.97 | 148 | < .001 | [-0.95, -0.90] |    Biserial
## Sepal.Length       | Species.versicolor |  0.10 |   1.25 | 148 | > .1   | [-0.06,  0.26] |    Biserial
## Sepal.Length       |  Species.virginica |  0.82 |  17.66 | 148 | < .001 | [ 0.77,  0.87] |    Biserial
## Sepal.Width        |       Petal.Length | -0.43 |  -5.77 | 148 | < .001 | [-0.55, -0.29] |     Pearson
## Sepal.Width        |        Petal.Width | -0.37 |  -4.79 | 148 | < .001 | [-0.50, -0.22] |     Pearson
## Sepal.Width        |     Species.setosa |  0.78 |  15.09 | 148 | < .001 | [ 0.71,  0.84] |    Biserial
## Sepal.Width        | Species.versicolor | -0.60 |  -9.20 | 148 | < .001 | [-0.70, -0.49] |    Biserial
## Sepal.Width        |  Species.virginica | -0.18 |  -2.16 | 148 | > .1   | [-0.33, -0.02] |    Biserial
## Petal.Length       |        Petal.Width |  0.96 |  43.39 | 148 | < .001 | [ 0.95,  0.97] |     Pearson
## Petal.Length       |     Species.setosa | -1.00 |   -Inf | 148 | < .001 | [-1.00, -1.00] |    Biserial
## Petal.Length       | Species.versicolor |  0.26 |   3.27 | 148 | < .01  | [ 0.10,  0.40] |    Biserial
## Petal.Length       |  Species.virginica |  0.93 |  31.09 | 148 | < .001 | [ 0.91,  0.95] |    Biserial
## Petal.Width        |     Species.setosa | -1.00 |   -Inf | 148 | < .001 | [-1.00, -1.00] |    Biserial
## Petal.Width        | Species.versicolor |  0.15 |   1.87 | 148 | > .1   | [-0.01,  0.31] |    Biserial
## Petal.Width        |  Species.virginica |  0.99 | 112.56 | 148 | < .001 | [ 0.99,  1.00] |    Biserial
## Species.setosa     | Species.versicolor | -0.88 | -22.35 | 148 | < .001 | [-0.91, -0.84] | Tetrachoric
## Species.setosa     |  Species.virginica | -0.88 | -22.35 | 148 | < .001 | [-0.91, -0.84] | Tetrachoric
## Species.versicolor |  Species.virginica | -0.88 | -22.35 | 148 | < .001 | [-0.91, -0.84] | Tetrachoric
```

## Partial Correlations

It also supports **partial correlations** (as well as Bayesian partial
correlations).

``` r
iris %>% 
  correlation(partial = TRUE) %>% 
  summary()
## Parameter    | Petal.Width | Petal.Length | Sepal.Width
## -------------------------------------------------------
## Sepal.Length |    -0.34*** |      0.72*** |     0.63***
## Sepal.Width  |     0.35*** |     -0.62*** |            
## Petal.Length |     0.87*** |              |
```

## Hierarchical (Partial) Correlations

It also provide some cutting, expolratory methods, such as Hierarchical
partial correlations. These are are partial correlations based on
**linear mixed models** that include the factors as random effects. They
can be see as (partial) correlations *adjusted* for some hierarchical
(or multilevel) variability.

``` r
iris %>% 
  correlation(include_factors = TRUE, partial_random = TRUE) %>% 
  summary()
## Parameter    | Petal.Width | Petal.Length | Sepal.Width
## -------------------------------------------------------
## Sepal.Length |     0.82*** |      0.87*** |       -0.12
## Sepal.Width  |    -0.37*** |     -0.43*** |            
## Petal.Length |     0.96*** |              |
```
