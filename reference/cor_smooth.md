# Smooth a non-positive definite correlation matrix to make it positive definite

Make correlations positive definite using
[`psych::cor.smooth`](https://rdrr.io/pkg/psych/man/cor.smooth.html). If
smoothing is done, inferential statistics (*p*-values, confidence
intervals, etc.) are removed, as they are no longer valid.

## Usage

``` r
cor_smooth(x, method = "psych", verbose = TRUE, ...)

is.positive_definite(x, tol = 10^-12, ...)

is_positive_definite(x, tol = 10^-12, ...)
```

## Arguments

- x:

  A correlation matrix.

- method:

  Smoothing method. Can be `psych` (will use
  [`psych::cor.smooth()`](https://rdrr.io/pkg/psych/man/cor.smooth.html)),
  `hj` (Jorjani et al., 2003) or `lrs` (Schaeffer, 2014). For the two
  last, will use
  [`mbend::bend()`](https://rdrr.io/pkg/mbend/man/bend.html) (check its
  documentation for details).

- verbose:

  Set to `FALSE` to silence the function.

- ...:

  Other arguments to be passed to or from other functions.

- tol:

  The minimum eigenvalue to be considered as acceptable.

## Examples

``` r
set.seed(123)
data <- as.matrix(mtcars)
# Make missing data so pairwise correlation matrix is non-positive definite
data[sample(seq_len(352), size = 60)] <- NA
data <- as.data.frame(data)
x <- correlation(data)
is.positive_definite(x)
#> [1] FALSE

smoothed <- cor_smooth(x)
#> mpg - cyl: -0.85 -> -0.82
#> 
#> mpg - disp: no change (-0.82)
#> 
#> mpg - hp: no change (-0.76)
#> 
#> mpg - drat: no change (0.58)
#> 
#> mpg - wt: no change (-0.84)
#> 
#> mpg - qsec: no change (0.32)
#> 
#> mpg - vs: no change (0.64)
#> 
#> mpg - am: 0.55 -> 0.54
#> 
#> mpg - gear: no change (0.42)
#> 
#> mpg - carb: no change (-0.57)
#> 
#> cyl - disp: 0.92 -> 0.89
#> 
#> cyl - hp: 0.88 -> 0.82
#> 
#> cyl - drat: -0.66 -> -0.64
#> 
#> cyl - wt: 0.80 -> 0.78
#> 
#> cyl - qsec: -0.47 -> -0.43
#> 
#> cyl - vs: -0.80 -> -0.76
#> 
#> cyl - am: -0.74 -> -0.66
#> 
#> cyl - gear: -0.66 -> -0.64
#> 
#> cyl - carb: no change (0.54)
#> 
#> disp - hp: no change (0.77)
#> 
#> disp - drat: no change (-0.57)
#> 
#> disp - wt: 0.88 -> 0.87
#> 
#> disp - qsec: -0.45 -> -0.43
#> 
#> disp - vs: no change (-0.74)
#> 
#> disp - am: -0.58 -> -0.56
#> 
#> disp - gear: no change (-0.52)
#> 
#> disp - carb: no change (0.56)
#> 
#> hp - drat: no change (-0.41)
#> 
#> hp - wt: 0.62 -> 0.60
#> 
#> hp - qsec: no change (-0.65)
#> 
#> hp - vs: no change (-0.71)
#> 
#> hp - am: -0.22 -> -0.23
#> 
#> hp - gear: no change (-0.14)
#> 
#> hp - carb: 0.65 -> 0.64
#> 
#> drat - wt: no change (-0.66)
#> 
#> drat - qsec: no change (-0.19)
#> 
#> drat - vs: no change (0.22)
#> 
#> drat - am: 0.74 -> 0.73
#> 
#> drat - gear: no change (0.71)
#> 
#> drat - carb: no change (-0.06)
#> 
#> wt - qsec: -0.09 -> -0.10
#> 
#> wt - vs: no change (-0.56)
#> 
#> wt - am: -0.72 -> -0.71
#> 
#> wt - gear: -0.64 -> -0.63
#> 
#> wt - carb: 0.50 -> 0.50
#> 
#> qsec - vs: no change (0.73)
#> 
#> qsec - am: -0.35 -> -0.32
#> 
#> qsec - gear: -0.13 -> -0.14
#> 
#> qsec - carb: -0.63 -> -0.61
#> 
#> vs - am: 0.20 -> 0.20
#> 
#> vs - gear: no change (0.32)
#> 
#> vs - carb: -0.69 -> -0.68
#> 
#> am - gear: 0.81 -> 0.79
#> 
#> am - carb: -0.24 -> -0.22
#> 
#> gear - carb: no change (0.03)
#> 
```
