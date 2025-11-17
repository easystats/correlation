# Convert a correlation to covariance

Convert a correlation to covariance

## Usage

``` r
cor_to_cov(cor, sd = NULL, variance = NULL, tol = .Machine$double.eps^(2/3))
```

## Arguments

- cor:

  A correlation matrix, or a partial or a semipartial correlation
  matrix.

- sd, variance:

  A vector that contains the standard deviations, or the variance, of
  the variables in the correlation matrix.

- tol:

  Relative tolerance to detect zero singular values.

## Value

A covariance matrix.

## Examples

``` r
cor <- cor(iris[1:4])
cov(iris[1:4])
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    0.6856935  -0.0424340    1.2743154   0.5162707
#> Sepal.Width    -0.0424340   0.1899794   -0.3296564  -0.1216394
#> Petal.Length    1.2743154  -0.3296564    3.1162779   1.2956094
#> Petal.Width     0.5162707  -0.1216394    1.2956094   0.5810063

cor_to_cov(cor, sd = sapply(iris[1:4], sd))
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    0.6856935  -0.0424340    1.2743154   0.5162707
#> Sepal.Width    -0.0424340   0.1899794   -0.3296564  -0.1216394
#> Petal.Length    1.2743154  -0.3296564    3.1162779   1.2956094
#> Petal.Width     0.5162707  -0.1216394    1.2956094   0.5810063
cor_to_cov(cor, variance = sapply(iris[1:4], var))
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    0.6856935  -0.0424340    1.2743154   0.5162707
#> Sepal.Width    -0.0424340   0.1899794   -0.3296564  -0.1216394
#> Petal.Length    1.2743154  -0.3296564    3.1162779   1.2956094
#> Petal.Width     0.5162707  -0.1216394    1.2956094   0.5810063
```
