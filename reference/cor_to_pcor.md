# Correlation Matrix to (Semi) Partial Correlations

Convert a correlation matrix to a (semi)partial correlation matrix.
Partial correlations are a measure of the correlation between two
variables that remains after controlling for (i.e., "partialling" out)
all the other relationships. They can be used for graphical Gaussian
models, as they represent the direct interactions between two variables,
conditioned on all remaining variables. This means that the squared
partial correlation between a predictor X1 and a response variable Y can
be interpreted as the proportion of (unique) variance accounted for by
X1 relative to the residual or unexplained variance of Y that cannot be
accounted by the other variables.

## Usage

``` r
cor_to_pcor(cor, tol = .Machine$double.eps^(2/3))

pcor_to_cor(pcor, tol = .Machine$double.eps^(2/3))

cor_to_spcor(cor = NULL, cov = NULL, tol = .Machine$double.eps^(2/3))
```

## Arguments

- cor:

  A correlation matrix, or a partial or a semipartial correlation
  matrix.

- tol:

  Relative tolerance to detect zero singular values.

- pcor:

  A correlation matrix, or a partial or a semipartial correlation
  matrix.

- cov:

  A covariance matrix (or a vector of the SD of the variables). Required
  for semi-partial correlations.

## Value

The (semi) partial correlation matrix.

## Details

The semi-partial correlation is similar to the partial correlation
statistic. However, it represents (when squared) the proportion of
(unique) variance accounted for by the predictor X1, relative to the
total variance of Y. Thus, it might be seen as a better indicator of the
"practical relevance" of a predictor, because it is scaled to (i.e.,
relative to) the total variability in the response variable.

## Examples

``` r
cor <- cor(iris[1:4])

# Partialize
cor_to_pcor(cor)
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    1.0000000   0.6285707    0.7190656  -0.3396174
#> Sepal.Width     0.6285707   1.0000000   -0.6152919   0.3526260
#> Petal.Length    0.7190656  -0.6152919    1.0000000   0.8707698
#> Petal.Width    -0.3396174   0.3526260    0.8707698   1.0000000
cor_to_spcor(cor, cov = sapply(iris[1:4], sd))
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length   1.00000000  0.30389212    0.3890689  -0.1357714
#> Sepal.Width    0.55758743  1.00000000   -0.5385056   0.2599849
#> Petal.Length   0.18506103 -0.13959991    1.0000000   0.3167424
#> Petal.Width   -0.09001634  0.09394365    0.4415000   1.0000000

# Inverse
round(pcor_to_cor(cor_to_pcor(cor)) - cor, 2) # Should be 0
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length            0           0            0           0
#> Sepal.Width             0           0            0           0
#> Petal.Length            0           0            0           0
#> Petal.Width             0           0            0           0
```
