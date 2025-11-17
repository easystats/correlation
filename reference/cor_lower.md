# Return the upper or lower triangular part

Return the upper or lower triangular part of the correlation matrix.

## Usage

``` r
cor_lower(x, diag = FALSE, ...)
```

## Arguments

- x:

  A correlation object.

- diag:

  Should the diagonal be included?

- ...:

  Other arguments to be passed to or from other functions.

## Examples

``` r
x <- correlation(mtcars, redundant = TRUE) # Generate full matrix
x <- cor_lower(x)

if (require("ggplot2")) {
  ggplot(x, aes(x = Parameter2, y = Parameter1, fill = r)) +
    geom_tile()
}
#> Loading required package: ggplot2


# Sorted
x <- correlation(mtcars, redundant = TRUE) # Generate full matrix
x <- cor_sort(x)
x <- cor_lower(x)

if (require("ggplot2")) {
  ggplot(x, aes(x = Parameter2, y = Parameter1, fill = r)) +
    geom_tile()
}
```
