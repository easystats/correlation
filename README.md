
# correlation <img src='man/figures/logo.png' align="right" height="139" />

[![CRAN](http://www.r-pkg.org/badges/version/correlation)](https://cran.r-project.org/package=correlation)
[![downloads](http://cranlogs.r-pkg.org/badges/correlation)](https://cran.r-project.org/package=correlation)
[![Build
Status](https://travis-ci.org/easystats/correlation.svg?branch=master)](https://travis-ci.org/easystats/correlation)
[![codecov](https://codecov.io/gh/easystats/correlation/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/correlation)

`correlation` is an
[**easystats**](https://github.com/easystats/easystats) package focused
on correlation analysis. It’s lightweight, easy to use, and allows for
the computation of many different kinds of correlations, such as
**partial** correlations, **Bayesian** correlations, **multilevel**
correlations, **polychoric** correlations, **biweight**, **percentage
bend** or **Sheperd’s Pi** correlations (types of robust correlation),
**distance** correlation (a type of non-linear correlation) and more,
also allowing for combinations between them (for instance, *Bayesian
partial multilevel correlation*).

You can reference the package and its documentation as follows:

  - Makowski, D., Ben-Shachar, M. S., Patil, I. & Lüdecke, D. (2020).
    *Methods for Correlation Analysis*. CRAN.

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

[![Documentation](https://img.shields.io/badge/documentation-correlation-orange.svg?colorB=E91E63)](https://easystats.github.io/correlation/)
[![Blog](https://img.shields.io/badge/blog-easystats-orange.svg?colorB=FF9800)](https://easystats.github.io/blog/posts/)
[![Features](https://img.shields.io/badge/features-correlation-orange.svg?colorB=2196F3)](https://easystats.github.io/correlation/reference/index.html)

Click on the buttons above to access the package
[**documentation**](https://easystats.github.io/correlation/) and the
[**easystats blog**](https://easystats.github.io/blog/posts/), and
check-out these vignettes:

  - [**Types of
    Correlation**](https://easystats.github.io/correlation/articles/types.html)
  - [**Multilevel
    Correlations**](https://easystats.github.io/correlation/articles/multilevel.html)

# Examples

The main function is
[`correlation()`](https://easystats.github.io/correlation/reference/correlation.html),
which builds on top of
[`cor_test()`](https://easystats.github.io/correlation/reference/cor_test.html)
and comes with a number of possible options.

## Correlation details and matrix

``` r
cor <- correlation(iris)
cor
## Parameter1   |   Parameter2 |     r |         95% CI |     t |  df |      p |  Method | n_Obs
## ---------------------------------------------------------------------------------------------
## Sepal.Length |  Sepal.Width | -0.12 | [-0.27,  0.04] | -1.44 | 148 | 0.152  | Pearson |   150
## Sepal.Length | Petal.Length |  0.87 | [ 0.83,  0.91] | 21.65 | 148 | < .001 | Pearson |   150
## Sepal.Length |  Petal.Width |  0.82 | [ 0.76,  0.86] | 17.30 | 148 | < .001 | Pearson |   150
## Sepal.Width  | Petal.Length | -0.43 | [-0.55, -0.29] | -5.77 | 148 | < .001 | Pearson |   150
## Sepal.Width  |  Petal.Width | -0.37 | [-0.50, -0.22] | -4.79 | 148 | < .001 | Pearson |   150
## Petal.Length |  Petal.Width |  0.96 | [ 0.95,  0.97] | 43.39 | 148 | < .001 | Pearson |   150
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

``` r
library(dplyr)
library(see)

cor %>% 
  as.table() %>% 
  plot()
```

![](man/figures/unnamed-chunk-7-1.png)<!-- -->

## Grouped dataframes

The function also supports **stratified correlations**, all within the
*tidyverse* workflow\!

``` r
iris %>% 
  select(Species, Sepal.Length, Sepal.Width, Petal.Width) %>% 
  group_by(Species) %>% 
  correlation()
## Group      |   Parameter1 |  Parameter2 |    r |        95% CI |    t | df |      p |  Method | n_Obs
## -----------------------------------------------------------------------------------------------------
## setosa     | Sepal.Length | Sepal.Width | 0.74 | [ 0.59, 0.85] | 7.68 | 48 | < .001 | Pearson |    50
## setosa     | Sepal.Length | Petal.Width | 0.28 | [ 0.00, 0.52] | 2.01 | 48 | 0.101  | Pearson |    50
## setosa     |  Sepal.Width | Petal.Width | 0.23 | [-0.05, 0.48] | 1.66 | 48 | 0.104  | Pearson |    50
## versicolor | Sepal.Length | Sepal.Width | 0.53 | [ 0.29, 0.70] | 4.28 | 48 | < .001 | Pearson |    50
## versicolor | Sepal.Length | Petal.Width | 0.55 | [ 0.32, 0.72] | 4.52 | 48 | < .001 | Pearson |    50
## versicolor |  Sepal.Width | Petal.Width | 0.66 | [ 0.47, 0.80] | 6.15 | 48 | < .001 | Pearson |    50
## virginica  | Sepal.Length | Sepal.Width | 0.46 | [ 0.20, 0.65] | 3.56 | 48 | 0.002  | Pearson |    50
## virginica  | Sepal.Length | Petal.Width | 0.28 | [ 0.00, 0.52] | 2.03 | 48 | 0.048  | Pearson |    50
## virginica  |  Sepal.Width | Petal.Width | 0.54 | [ 0.31, 0.71] | 4.42 | 48 | < .001 | Pearson |    50
```

## Bayesian Correlations

It is very easy to switch to a **Bayesian framework**.

``` r
correlation(iris, bayesian = TRUE)
## Parameter1   |   Parameter2 |   rho |         95% CI |     pd | % in ROPE |    BF |              Prior | n_Obs
## --------------------------------------------------------------------------------------------------------------
## Sepal.Length |  Sepal.Width | -0.12 | [-0.24,  0.01] | 91.83% |    43.08% |  0.51 | Cauchy (0 +- 0.33) |   150
## Sepal.Length | Petal.Length |  0.86 | [ 0.83,  0.90] |   100% |        0% | > 999 | Cauchy (0 +- 0.33) |   150
## Sepal.Length |  Petal.Width |  0.80 | [ 0.76,  0.85] |   100% |        0% | > 999 | Cauchy (0 +- 0.33) |   150
## Sepal.Width  | Petal.Length | -0.42 | [-0.52, -0.31] |   100% |        0% | > 999 | Cauchy (0 +- 0.33) |   150
## Sepal.Width  |  Petal.Width | -0.35 | [-0.47, -0.25] |   100% |     0.02% | > 999 | Cauchy (0 +- 0.33) |   150
## Petal.Length |  Petal.Width |  0.96 | [ 0.95,  0.97] |   100% |        0% | > 999 | Cauchy (0 +- 0.33) |   150
```

## Tetrachoric, Polychoric, Biserial, Biweight…

The `correlation` package also supports different types of methods,
which can deal with correlations **between factors**\!

``` r
correlation(iris, include_factors = TRUE, method = "auto")
## Parameter1         |         Parameter2 |     r |         95% CI |      t |  df |      p |         Method | n_Obs
## -----------------------------------------------------------------------------------------------------------------
## Sepal.Length       |        Sepal.Width | -0.12 | [-0.27,  0.04] |  -1.44 | 148 | 0.452  |        Pearson |   150
## Sepal.Length       |       Petal.Length |  0.87 | [ 0.83,  0.91] |  21.65 | 148 | < .001 |        Pearson |   150
## Sepal.Length       |        Petal.Width |  0.82 | [ 0.76,  0.86] |  17.30 | 148 | < .001 |        Pearson |   150
## Sepal.Length       |     Species.setosa | -0.72 | [-0.79, -0.63] | -12.53 | 148 | < .001 | Point-biserial |   150
## Sepal.Length       | Species.versicolor |  0.08 | [-0.08,  0.24] |   0.97 | 148 | 0.452  | Point-biserial |   150
## Sepal.Length       |  Species.virginica |  0.64 | [ 0.53,  0.72] |  10.08 | 148 | < .001 | Point-biserial |   150
## Sepal.Width        |       Petal.Length | -0.43 | [-0.55, -0.29] |  -5.77 | 148 | < .001 |        Pearson |   150
## Sepal.Width        |        Petal.Width | -0.37 | [-0.50, -0.22] |  -4.79 | 148 | < .001 |        Pearson |   150
## Sepal.Width        |     Species.setosa |  0.60 | [ 0.49,  0.70] |   9.20 | 148 | < .001 | Point-biserial |   150
## Sepal.Width        | Species.versicolor | -0.47 | [-0.58, -0.33] |  -6.44 | 148 | < .001 | Point-biserial |   150
## Sepal.Width        |  Species.virginica | -0.14 | [-0.29,  0.03] |  -1.67 | 148 | 0.392  | Point-biserial |   150
## Petal.Length       |        Petal.Width |  0.96 | [ 0.95,  0.97] |  43.39 | 148 | < .001 |        Pearson |   150
## Petal.Length       |     Species.setosa | -0.92 | [-0.94, -0.89] | -29.13 | 148 | < .001 | Point-biserial |   150
## Petal.Length       | Species.versicolor |  0.20 | [ 0.04,  0.35] |   2.51 | 148 | 0.066  | Point-biserial |   150
## Petal.Length       |  Species.virginica |  0.72 | [ 0.63,  0.79] |  12.66 | 148 | < .001 | Point-biserial |   150
## Petal.Width        |     Species.setosa | -0.89 | [-0.92, -0.85] | -23.41 | 148 | < .001 | Point-biserial |   150
## Petal.Width        | Species.versicolor |  0.12 | [-0.04,  0.27] |   1.44 | 148 | 0.452  | Point-biserial |   150
## Petal.Width        |  Species.virginica |  0.77 | [ 0.69,  0.83] |  14.66 | 148 | < .001 | Point-biserial |   150
## Species.setosa     | Species.versicolor | -0.88 | [-0.91, -0.84] | -22.35 | 148 | < .001 |    Tetrachoric |   150
## Species.setosa     |  Species.virginica | -0.88 | [-0.91, -0.84] | -22.35 | 148 | < .001 |    Tetrachoric |   150
## Species.versicolor |  Species.virginica | -0.88 | [-0.91, -0.84] | -22.35 | 148 | < .001 |    Tetrachoric |   150
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

## Gaussian Graphical Models (GGMs)

Such partial correlations can also be represented as **Gaussian
graphical models**, an increasingly popular tool in psychology:

``` r
library(see) # for plotting
library(ggraph) # needs to be loaded

mtcars %>% 
  correlation(partial = TRUE) %>% 
  plot()
```

![](man/figures/unnamed-chunk-12-1.png)<!-- -->

## Multilevel Correlations

It also provide some cutting-edge methods, such as Multilevel (partial)
correlations. These are are partial correlations based on **linear mixed
models** that include the factors as random effects. They can be see as
correlations *adjusted* for some group (*hierarchical*) variability.

``` r
iris %>% 
  correlation(partial = TRUE, multilevel = TRUE) %>% 
  summary()
## Parameter    | Petal.Width | Petal.Length | Sepal.Width
## -------------------------------------------------------
## Sepal.Length |      -0.17* |      0.71*** |     0.43***
## Sepal.Width  |     0.39*** |       -0.18* |            
## Petal.Length |     0.38*** |              |
```

However, if the `partial` argument is set to `FALSE`, it will try to
convert the partial coefficient into regular ones.These can be
**converted back** to full correlations:

``` r
iris %>% 
  correlation(partial = FALSE, multilevel = TRUE) %>% 
  summary()
## Parameter    | Petal.Width | Petal.Length | Sepal.Width
## -------------------------------------------------------
## Sepal.Length |     0.36*** |      0.76*** |     0.53***
## Sepal.Width  |     0.47*** |      0.38*** |            
## Petal.Length |     0.48*** |              |
```
