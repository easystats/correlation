# Convert correlation to p-values and CIs

Get statistics, *p*-values and confidence intervals (CI) from
correlation coefficients.

## Usage

``` r
cor_to_ci(cor, n, ci = 0.95, method = "pearson", correction = "fieller", ...)

cor_to_p(cor, n, method = "pearson")
```

## Arguments

- cor:

  A correlation matrix or coefficient.

- n:

  The sample size (number of observations).

- ci:

  Confidence/Credible Interval level. If `"default"`, then it is set to
  `0.95` (`95%` CI).

- method:

  A character string indicating which correlation coefficient is to be
  used for the test. One of `"pearson"` (default), `"kendall"`,
  `"spearman"` (but see also the `robust` argument), `"biserial"`,
  `"polychoric"`, `"tetrachoric"`, `"biweight"`, `"distance"`,
  `"percentage"` (for percentage bend correlation), `"blomqvist"` (for
  Blomqvist's coefficient), `"hoeffding"` (for Hoeffding's D),
  `"gamma"`, `"gaussian"` (for Gaussian Rank correlation) or
  `"shepherd"` (for Shepherd's Pi correlation). Setting `"auto"` will
  attempt at selecting the most relevant method (polychoric when ordinal
  factors involved, tetrachoric when dichotomous factors involved,
  point-biserial if one dichotomous and one continuous and pearson
  otherwise). See below the **details** section for a description of
  these indices.

- correction:

  Only used if method is 'spearman' or 'kendall'. Can be 'fieller'
  (default; Fieller et al., 1957), 'bw' (only for Spearman) or 'none'.
  Bonett and Wright (2000) claim their correction ('bw') performs
  better, though the Bishara and Hittner (2017) paper favours the
  Fieller correction. Both are generally very similar.

- ...:

  Additional arguments (e.g., `alternative`) to be passed to other
  methods. See
  [`stats::cor.test`](https://rdrr.io/r/stats/cor.test.html) for further
  details.

## Value

A list containing a *p*-value and the statistic or the CI bounds.

## References

Bishara, A. J., & Hittner, J. B. (2017). Confidence intervals for
correlations when data are not normal. Behavior research methods, 49(1),
294-309.

## Examples

``` r
cor.test(iris$Sepal.Length, iris$Sepal.Width)
#> 
#>  Pearson's product-moment correlation
#> 
#> data:  iris$Sepal.Length and iris$Sepal.Width
#> t = -1.4403, df = 148, p-value = 0.1519
#> alternative hypothesis: true correlation is not equal to 0
#> 95 percent confidence interval:
#>  -0.27269325  0.04351158
#> sample estimates:
#>        cor 
#> -0.1175698 
#> 
cor_to_p(-0.1175698, n = 150)
#> $p
#> [1] 0.1518982
#> 
#> $statistic
#> [1] -1.440287
#> 
cor_to_p(cor(iris[1:4]), n = 150)
#> $p
#>              Sepal.Length  Sepal.Width Petal.Length  Petal.Width
#> Sepal.Length 0.000000e+00 1.518983e-01 1.038667e-47 2.325498e-37
#> Sepal.Width  1.518983e-01 0.000000e+00 4.513314e-08 4.073229e-06
#> Petal.Length 1.038667e-47 4.513314e-08 0.000000e+00 4.675004e-86
#> Petal.Width  2.325498e-37 4.073229e-06 4.675004e-86 0.000000e+00
#> 
#> $statistic
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length          Inf   -1.440287    21.646019   17.296454
#> Sepal.Width     -1.440287         Inf    -5.768449   -4.786461
#> Petal.Length    21.646019   -5.768449          Inf   43.387237
#> Petal.Width     17.296454   -4.786461    43.387237         Inf
#> 
cor_to_ci(-0.1175698, n = 150)
#> $CI_low
#> [1] -0.2726933
#> 
#> $CI_high
#> [1] 0.04351157
#> 
cor_to_ci(cor(iris[1:4]), n = 150)
#> $CI_low
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length    1.0000000  -0.2726932    0.8270363   0.7568971
#> Sepal.Width    -0.2726932   1.0000000   -0.5508771  -0.4972130
#> Petal.Length    0.8270363  -0.5508771    1.0000000   0.9490525
#> Petal.Width     0.7568971  -0.4972130    0.9490525   1.0000000
#> 
#> $CI_high
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length   1.00000000  0.04351158    0.9055080   0.8648361
#> Sepal.Width    0.04351158  1.00000000   -0.2879499  -0.2186966
#> Petal.Length   0.90550805 -0.28794993    1.0000000   0.9729853
#> Petal.Width    0.86483606 -0.21869663    0.9729853   1.0000000
#> 

cor.test(iris$Sepal.Length, iris$Sepal.Width, method = "spearman", exact = FALSE)
#> 
#>  Spearman's rank correlation rho
#> 
#> data:  iris$Sepal.Length and iris$Sepal.Width
#> S = 656283, p-value = 0.04137
#> alternative hypothesis: true rho is not equal to 0
#> sample estimates:
#>        rho 
#> -0.1667777 
#> 
cor_to_p(-0.1667777, n = 150, method = "spearman")
#> $p
#> [1] 0.04136794
#> 
#> $statistic
#> [1] -2.057758
#> 
cor_to_ci(-0.1667777, ci = 0.95, n = 150)
#> $CI_low
#> [1] -0.3185257
#> 
#> $CI_high
#> [1] -0.006695121
#> 

cor.test(iris$Sepal.Length, iris$Sepal.Width, method = "kendall", exact = FALSE)
#> 
#>  Kendall's rank correlation tau
#> 
#> data:  iris$Sepal.Length and iris$Sepal.Width
#> z = -1.3318, p-value = 0.1829
#> alternative hypothesis: true tau is not equal to 0
#> sample estimates:
#>         tau 
#> -0.07699679 
#> 
cor_to_p(-0.07699679, n = 150, method = "kendall")
#> p-value estimation for Kendall's correlation is not perfectly correct.
#>   Help us to improve it.
#> $p
#> [1] 0.1620545
#> 
#> $statistic
#> [1] -1.398195
#> 
```
