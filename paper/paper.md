---
title: 'Methods for Correlation Analysis'
tags:
- R
- Correlation
- Easystats
authors:
- affiliation: 1
  name: Dominique Makowski
  orcid: 0000-0001-5375-9967
- affiliation: 2
  name: Mattan S. Ben-Shachar
  orcid: 0000-0002-4287-4801
- affiliation: 3
  name: Indrajeet Patil
  orcid: 0000-0003-1995-6531
- affiliation: 4
  name: Daniel Lüdecke
  orcid: 0000-0002-8895-3206
affiliations:
- index: 1
  name: Nanyang Technological University, Singapore
- index: 2
  name: Ben-Gurion University of the Negev, Israel
- index: 3
  name: Max Planck Institute for Human Development, Germany
- index: 4
  name: University Medical Center Hamburg-Eppendorf, Germany
date: "22 March 2020"
bibliography: paper.bib
csl: apa.csl
output: pdf_document
---

# Introduction

Correlations tests are arguably one of the most commonly used statistical procedures, and are used as a basis in many applications such as exploratory data analysis, structural modeling, data engineering etc.

**correlation** is a an R package [@Rteam] package, from the [**easystats**](https://github.com/easystats/easystats) collection, focused on correlation analysis. It's lightweight, easy to use, and allows for the computation of many different kinds of correlations.



# Features

![figure1](figure1.PNG)

- **Pearson's correlation**: The covariance of the two variables divided by the product of their standard deviations.

- **Spearman's rank correlation**: A nonparametric measure of rank correlation (statistical dependence between the rankings of two variables). The Spearman correlation between two variables is equal to the Pearson correlation between the rank values of those two variables; while Pearson's correlation assesses linear relationships, Spearman's correlation assesses monotonic relationships (whether linear or not).

- **Kendall's rank correlation**: In the normal case, the Kendall correlation is preferred than the Spearman correlation because of a smaller gross error sensitivity (GES) and a smaller asymptotic variance (AV), making it more robust and more efficient. However, the interpretation of Kendall's tau is less direct than that of Spearman's rho, in the sense that it quantifies the difference between the % of concordant and discordant pairs among all possible pairwise events.

- **Biweight midcorrelation**: A measure of similarity between samples that is median-based, rather than mean-based, thus is less sensitive to outliers, and can be a robust alternative to other similarity metrics, such as Pearson correlation.

- **Distance correlation**: Distance correlation measures both linear and nonlinear association between two random variables or random vectors. This is in contrast to Pearson's correlation, which can only detect linear association between two random variables.

- **Percentage bend correlation**: Introduced by Wilcox (1994), it is based on a down-weight of a specified percentage of marginal observations deviating from the median (by default, 20%).

- **Shepherd's Pi correlation**: Equivalent to a Spearman's rank correlation after outliers removal (by means of bootstrapped mahalanobis distance).

- **Point-Biserial and biserial correlation**: Correlation coefficient used when one variable is continuous and the other is dichotomous (binary). Point-serial is equivalent to a Pearson's correlation, while Biserial should be used when the binary variable is assumed to have an underlying continuity. For example, depression level can be measured on a continuous scale, but can be classified dichotomously as high/low.

- **Polychoric correlation**: Correlation between two theorised normally distributed continuous latent variables, from two observed ordinal variables.

- **Tetrachoric correlation**: Special case of the polychoric correlation applicable when both observed variables are dichotomous.

- **Partial correlation**: something

- **Partial correlation**: something

- **Multilevel correlation**: something


# Design


It relies on one main function, `correlation()`, which outputs a dataframe containing each pairwise correlation per row. This long format is convenient for further data analysis, but not as much to get a summary, which is usually obtained via a correlation matrix. To address this, we added standard methods, such as `summary()` and `as.table()`, to automatically transform the long output to a matrix. Moreover, **correlation** also include plotting capabilities via the [**see** package](https://easystats.github.io/see/) [@ludecke2019see].

# Examples

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
  as.matrix() %>% 
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
correlation(iris, bayesian=TRUE)
## Parameter1   |   Parameter2 |   rho |         95% CI |     pd | % in ROPE |    BF |              Prior | n_Obs
## --------------------------------------------------------------------------------------------------------------
## Sepal.Length |  Sepal.Width | -0.11 | [-0.24,  0.01] | 92.22% |    43.10% |  0.51 | Cauchy (0 +- 0.33) |   150
## Sepal.Length | Petal.Length |  0.86 | [ 0.83,  0.89] |   100% |        0% | > 999 | Cauchy (0 +- 0.33) |   150
## Sepal.Length |  Petal.Width |  0.80 | [ 0.76,  0.85] |   100% |        0% | > 999 | Cauchy (0 +- 0.33) |   150
## Sepal.Width  | Petal.Length | -0.42 | [-0.53, -0.31] |   100% |        0% | > 999 | Cauchy (0 +- 0.33) |   150
## Sepal.Width  |  Petal.Width | -0.35 | [-0.46, -0.23] |   100% |     0.02% | > 999 | Cauchy (0 +- 0.33) |   150
## Petal.Length |  Petal.Width |  0.96 | [ 0.95,  0.97] |   100% |        0% | > 999 | Cauchy (0 +- 0.33) |   150
```

## Tetrachoric, Polychoric, Biserial, Biweight…

The `correlation` package also supports different types of methods,
which can deal with correlations **between factors**\!

``` r
correlation(iris, include_factors = TRUE, method = "auto")
## Parameter1         |         Parameter2 |     r |         95% CI |      t |  df |      p |      Method | n_Obs
## --------------------------------------------------------------------------------------------------------------
## Sepal.Length       |        Sepal.Width | -0.12 | [-0.27,  0.04] |  -1.44 | 148 | 0.304  |     Pearson |   150
## Sepal.Length       |       Petal.Length |  0.87 | [ 0.83,  0.91] |  21.65 | 148 | < .001 |     Pearson |   150
## Sepal.Length       |        Petal.Width |  0.82 | [ 0.76,  0.86] |  17.30 | 148 | < .001 |     Pearson |   150
## Sepal.Length       |     Species.setosa | -0.93 | [-0.95, -0.90] | -29.97 | 148 | < .001 |    Biserial |   150
## Sepal.Length       | Species.versicolor |  0.10 | [-0.06,  0.26] |   1.25 | 148 | 0.304  |    Biserial |   150
## Sepal.Length       |  Species.virginica |  0.82 | [ 0.77,  0.87] |  17.66 | 148 | < .001 |    Biserial |   150
## Sepal.Width        |       Petal.Length | -0.43 | [-0.55, -0.29] |  -5.77 | 148 | < .001 |     Pearson |   150
## Sepal.Width        |        Petal.Width | -0.37 | [-0.50, -0.22] |  -4.79 | 148 | < .001 |     Pearson |   150
## Sepal.Width        |     Species.setosa |  0.78 | [ 0.71,  0.84] |  15.09 | 148 | < .001 |    Biserial |   150
## Sepal.Width        | Species.versicolor | -0.60 | [-0.70, -0.49] |  -9.20 | 148 | < .001 |    Biserial |   150
## Sepal.Width        |  Species.virginica | -0.18 | [-0.33, -0.02] |  -2.16 | 148 | 0.130  |    Biserial |   150
## Petal.Length       |        Petal.Width |  0.96 | [ 0.95,  0.97] |  43.39 | 148 | < .001 |     Pearson |   150
## Petal.Length       |     Species.setosa | -1.00 | [-1.00, -1.00] |   -Inf | 148 | < .001 |    Biserial |   150
## Petal.Length       | Species.versicolor |  0.26 | [ 0.10,  0.40] |   3.27 | 148 | 0.007  |    Biserial |   150
## Petal.Length       |  Species.virginica |  0.93 | [ 0.91,  0.95] |  31.09 | 148 | < .001 |    Biserial |   150
## Petal.Width        |     Species.setosa | -1.00 | [-1.00, -1.00] |   -Inf | 148 | < .001 |    Biserial |   150
## Petal.Width        | Species.versicolor |  0.15 | [-0.01,  0.31] |   1.87 | 148 | 0.191  |    Biserial |   150
## Petal.Width        |  Species.virginica |  0.99 | [ 0.99,  1.00] | 112.56 | 148 | < .001 |    Biserial |   150
## Species.setosa     | Species.versicolor | -0.88 | [-0.91, -0.84] | -22.35 | 148 | < .001 | Tetrachoric |   150
## Species.setosa     |  Species.virginica | -0.88 | [-0.91, -0.84] | -22.35 | 148 | < .001 | Tetrachoric |   150
## Species.versicolor |  Species.virginica | -0.88 | [-0.91, -0.84] | -22.35 | 148 | < .001 | Tetrachoric |   150
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

![](man/figures/unnamed-chunk-12-1.png)


# Licensing and Availability

The **correlation** package can be downloaded and installed from CRAN [1](https://CRAN.R-project.org/package=correlation). It is licensed under the GNU General Public License (v3.0), with all its source code stored at GitHub [2](https://github.com/easystats/correlation), and with a corresponding issue tracker [2](https://github.com/easystats/correlation/issues) for bug reporting and feature enhancements. In the spirit of honest and open science, we encourage requests/tips for fixes, feature updates, as well as general questions and concerns via direct interaction with contributors and developers.

# Acknowledgments

**correlation** is part of the [*easystats*](https://github.com/easystats/easystats) ecosystem [relying on **insight**; @ludecke2019insight and **bayestestR**; @makowski2019bayestestr], a collaborative project created to facilitate the usage of R. Thus, we would like to thank the [council of masters](https://github.com/orgs/easystats/people) of easystats, all other padawan contributors, as well as the users.

# References
