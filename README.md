
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

The package documentation can be found
[**here**](https://easystats.github.io/correlation/).

# Features

The main function is `correlation()`, which comes with a number of
possible
parameters.

## Correlations

``` r
correlation(iris)
```

| Parameter1   | Parameter2   |      r |      t | DoF |   p | CI\_low | CI\_high | CI\_level | Method  |
| :----------- | :----------- | -----: | -----: | --: | --: | ------: | -------: | --------: | :------ |
| Sepal.Length | Sepal.Length |   1.00 |    Inf | 148 | 0.0 |    1.00 |     1.00 |      0.95 | Pearson |
| Sepal.Width  | Sepal.Length | \-0.12 | \-1.44 | 148 | 0.3 |  \-0.27 |     0.04 |      0.95 | Pearson |
| Petal.Length | Sepal.Length |   0.87 |  21.65 | 148 | 0.0 |    0.83 |     0.91 |      0.95 | Pearson |
| Petal.Width  | Sepal.Length |   0.82 |  17.30 | 148 | 0.0 |    0.76 |     0.86 |      0.95 | Pearson |
| Sepal.Length | Sepal.Width  | \-0.12 | \-1.44 | 148 | 0.3 |  \-0.27 |     0.04 |      0.95 | Pearson |
| Sepal.Width  | Sepal.Width  |   1.00 |    Inf | 148 | 0.0 |    1.00 |     1.00 |      0.95 | Pearson |

The output is not a square matrix, but a long dataframe with all
correlations tests per row.

## Grouped Dataframes

This comes with the advantage of being compatible with the tidyverse
workflow.

``` r
library(dplyr)

iris %>% 
  select(Species, starts_with("Sepal")) %>% 
  group_by(Species) %>% 
  correlation() %>% 
  filter(r < 0.9)
```

| Group      | Parameter1   | Parameter2   |    r |    t | DoF | p | CI\_low | CI\_high | CI\_level | Method  |
| :--------- | :----------- | :----------- | ---: | ---: | --: | -: | ------: | -------: | --------: | :------ |
| setosa     | Sepal.Width  | Sepal.Length | 0.74 | 7.68 |  48 | 0 |    0.59 |     0.85 |      0.95 | Pearson |
| setosa     | Sepal.Length | Sepal.Width  | 0.74 | 7.68 |  48 | 0 |    0.59 |     0.85 |      0.95 | Pearson |
| versicolor | Sepal.Width  | Sepal.Length | 0.53 | 4.28 |  48 | 0 |    0.29 |     0.70 |      0.95 | Pearson |
| versicolor | Sepal.Length | Sepal.Width  | 0.53 | 4.28 |  48 | 0 |    0.29 |     0.70 |      0.95 | Pearson |
| virginica  | Sepal.Width  | Sepal.Length | 0.46 | 3.56 |  48 | 0 |    0.20 |     0.65 |      0.95 | Pearson |
| virginica  | Sepal.Length | Sepal.Width  | 0.46 | 3.56 |  48 | 0 |    0.20 |     0.65 |      0.95 | Pearson |

## Partial and Semi-partial Correlations

It also supports **partial** and **semi-partial** correlations.

``` r
correlation(select(iris, Species, starts_with("Sepal")),
            select(iris, Species, starts_with("Petal")),
            partial=TRUE)
```

| Parameter1   | Parameter2   |      r |      t | p |
| :----------- | :----------- | -----: | -----: | -: |
| Sepal.Length | Petal.Length |   0.72 |  12.50 | 0 |
| Sepal.Length | Petal.Width  | \-0.34 | \-4.36 | 0 |
| Sepal.Width  | Petal.Length | \-0.62 | \-9.43 | 0 |
| Sepal.Width  | Petal.Width  |   0.35 |   4.55 | 0 |

## Bayesian Correlations

It is very easy to switch to a Bayesian
framework.

``` r
correlation(iris, bayesian=TRUE)
```

| Parameter1   | Parameter2   | Median |  MAD | CI\_low | CI\_high |     pd | ROPE\_Percentage |           BF | Prior  |
| :----------- | :----------- | -----: | ---: | ------: | -------: | -----: | ---------------: | -----------: | :----- |
| Sepal.Length | Sepal.Length |   1.00 | 0.00 |    1.00 |     1.00 | 100.00 |             0.00 |          Inf | medium |
| Sepal.Width  | Sepal.Length | \-0.11 | 0.08 |  \-0.24 |     0.02 |  91.58 |            19.64 | 5.100000e-01 | medium |
| Petal.Length | Sepal.Length |   0.86 | 0.02 |    0.83 |     0.90 | 100.00 |             0.00 | 2.136483e+43 | medium |
| Petal.Width  | Sepal.Length |   0.81 | 0.03 |    0.76 |     0.85 | 100.00 |             0.00 | 2.621977e+33 | medium |
| Sepal.Length | Sepal.Width  | \-0.11 | 0.08 |  \-0.24 |     0.02 |  91.72 |            19.19 | 5.100000e-01 | medium |
| Sepal.Width  | Sepal.Width  |   1.00 | 0.00 |    1.00 |     1.00 | 100.00 |             0.00 |          Inf | medium |

## Reports and tables

The correlations are also compatible with the
[report](https://github.com/easystats/report) package to produce text…

``` r
library(report)
library(report)
iris %>% 
  select(starts_with("Sepal")) %>% 
  correlation(bayesian=TRUE) %>% 
  report()
```

    ## We ran a Bayesian correlation analysis (prior scale set to medium). The Region of Practical Equivalence (ROPE) percentage was defined as the proportion of the posterior distribution within the [-0.05, 0.05] range. Effect sizes were labelled following Cohen's (1988) recommendations.
    ## 
    ##   - There is extreme evidence (BF > 999) in favour of a positive and large correlation between Sepal.Length and Sepal.Length (r's median = 1.00, 90% CI [1.00, 1.00], pd = 100%, 0.00% in ROPE).
    ##   - There is anecdotal evidence (BF = 0.51) against a negative and small correlation between Sepal.Width and Sepal.Length (r's median = -0.11, 90% CI [-0.24, 0.02], pd = 92.16%, 19.59% in ROPE).
    ##   - There is anecdotal evidence (BF = 0.51) against a negative and small correlation between Sepal.Length and Sepal.Width (r's median = -0.11, 90% CI [-0.24, 0.02], pd = 92.07%, 20.13% in ROPE).
    ##   - There is extreme evidence (BF > 999) in favour of a positive and large correlation between Sepal.Width and Sepal.Width (r's median = 1.00, 90% CI [1.00, 1.00], pd = 100%, 0.00% in ROPE).

Or tables:

``` r
iris %>% 
  group_by(Species) %>% 
  correlation() %>% 
  report() %>% 
  to_table()
```

|    | Group      | Parameter    | Petal.Length | Petal.Width | Sepal.Length |
| -- | :--------- | :----------- | :----------- | :---------- | :----------- |
| 2  | setosa     | Petal.Width  | 0.33         |             |              |
| 3  | setosa     | Sepal.Length | 0.27         | 0.28        |              |
| 4  | setosa     | Sepal.Width  | 0.18         | 0.23        | 0.74\*\*\*   |
| 7  | versicolor | Petal.Width  | 0.79\*\*\*   |             |              |
| 8  | versicolor | Sepal.Length | 0.75\*\*\*   | 0.55\*\*\*  |              |
| 9  | versicolor | Sepal.Width  | 0.56\*\*\*   | 0.66\*\*\*  | 0.53\*\*\*   |
| 12 | virginica  | Petal.Width  | 0.32         |             |              |
| 13 | virginica  | Sepal.Length | 0.86\*\*\*   | 0.28        |              |
| 14 | virginica  | Sepal.Width  | 0.40\*       | 0.54\*\*\*  | 0.46\*\*     |
