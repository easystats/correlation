# Export tables into different output formats

Export tables (i.e. data frame) into different output formats.
[`print_md()`](https://easystats.github.io/insight/reference/display.html)
is a alias for `display(format = "markdown")`. Note that you can use
[`format()`](https://rdrr.io/r/base/format.html) to get the formatted
table as a dataframe.

## Usage

``` r
# S3 method for class 'easycormatrix'
display(
  object,
  format = "markdown",
  digits = 2,
  p_digits = 3,
  stars = TRUE,
  include_significance = NULL,
  ...
)

# S3 method for class 'easycorrelation'
print_md(x, digits = NULL, p_digits = NULL, stars = NULL, ...)

# S3 method for class 'easycorrelation'
print_html(x, digits = NULL, p_digits = NULL, stars = NULL, ...)

# S3 method for class 'easycormatrix'
print_md(
  x,
  digits = NULL,
  p_digits = NULL,
  stars = NULL,
  include_significance = NULL,
  ...
)

# S3 method for class 'easycormatrix'
print_html(
  x,
  digits = NULL,
  p_digits = NULL,
  stars = NULL,
  include_significance = NULL,
  ...
)
```

## Arguments

- object, x:

  An object returned by
  [`correlation()`](https://easystats.github.io/correlation/reference/correlation.md)
  or its summary.

- format:

  String, indicating the output format. Currently, only `"markdown"` is
  supported.

- digits, p_digits:

  To do...

- stars:

  To do...

- include_significance:

  To do...

- ...:

  Currently not used.

## Value

A character vector. If `format = "markdown"`, the return value will be a
character vector in markdown-table format.

## Details

[`display()`](https://easystats.github.io/insight/reference/display.html)
is useful when the table-output from functions, which is usually printed
as formatted text-table to console, should be formatted for pretty
table-rendering in markdown documents, or if knitted from rmarkdown to
PDF or Word files.

## Examples

``` r
data(iris)
corr <- correlation(iris)
display(corr)
#> 
#> 
#> Table: Correlation Matrix (pearson-method)
#> 
#> |Parameter1   |   Parameter2 |     r |         95% CI | t(148) |         p |
#> |:------------|:------------:|:-----:|:--------------:|:------:|:---------:|
#> |Sepal.Length |  Sepal.Width | -0.12 |  (-0.27, 0.04) |  -1.44 | 0.152     |
#> |Sepal.Length | Petal.Length |  0.87 |   (0.83, 0.91) |  21.65 | < .001*** |
#> |Sepal.Length |  Petal.Width |  0.82 |   (0.76, 0.86) |  17.30 | < .001*** |
#> |Sepal.Width  | Petal.Length | -0.43 | (-0.55, -0.29) |  -5.77 | < .001*** |
#> |Sepal.Width  |  Petal.Width | -0.37 | (-0.50, -0.22) |  -4.79 | < .001*** |
#> |Petal.Length |  Petal.Width |  0.96 |   (0.95, 0.97) |  43.39 | < .001*** |
#> p-value adjustment method: Holm (1979)
#> Observations: 150

s <- summary(corr)
display(s)
#> 
#> 
#> Table: Correlation Matrix (pearson-method)
#> 
#> |Parameter    | Petal.Width | Petal.Length | Sepal.Width |
#> |:------------|:-----------:|:------------:|:-----------:|
#> |Sepal.Length |     0.82*** |      0.87*** |       -0.12 |
#> |Sepal.Width  |    -0.37*** |     -0.43*** |             |
#> |Petal.Length |     0.96*** |              |             |
#> p-value adjustment method: Holm (1979)
```
