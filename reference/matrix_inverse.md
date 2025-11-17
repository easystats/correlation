# Matrix Inversion

Performs a Moore-Penrose generalized inverse (also called the
Pseudoinverse).

## Usage

``` r
matrix_inverse(m, tol = .Machine$double.eps^(2/3))
```

## Arguments

- m:

  Matrix for which the inverse is required.

- tol:

  Relative tolerance to detect zero singular values.

## Value

An inversed matrix.

## See also

pinv from the pracma package

## Examples

``` r
m <- cor(iris[1:4])
matrix_inverse(m)
#>              Sepal.Length Sepal.Width Petal.Length Petal.Width
#> Sepal.Length     7.072722   -2.422965   -10.692191    3.622961
#> Sepal.Width     -2.422965    2.100872     4.986386   -2.050192
#> Petal.Length   -10.692191    4.986386    31.261498  -19.529388
#> Petal.Width      3.622961   -2.050192   -19.529388   16.090175
```
