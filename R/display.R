#' @title Export tables into different output formats
#' @name display.easycormatrix
#'
#' @description Export tables (i.e. data frame) into different output formats.
#'   `print_md()` is a alias for `display(format = "markdown")`. Note that
#'   you can use `format()` to get the formatted table as a dataframe.
#'
#' @param object,x An object returned by
#'   [`correlation()`][correlation] or its summary.
#' @param format String, indicating the output format. Can be `"markdown"`
#'   (default) or `"html"`.
#' @param digits,p_digits Number of digits used for the correlation
#'   coefficients and for the significance values. `p_digits` also accepts
#'   `"apa"`, to format p-values in APA style.
#' @param stars Logical, if `TRUE`, significance stars are added to the
#'   coefficients.
#' @param include_significance Logical, if `TRUE`, the significance values
#'   themselves (p-values, probability of direction or Bayes factors, depending
#'   on the correlation) are shown in parentheses next to the coefficients. If
#'   `FALSE` and `stars = TRUE`, only the stars are shown.
#' @param ... Currently not used.
#'
#' @return A character vector. If `format = "markdown"`, the return value
#'   will be a character vector in markdown-table format.
#'
#' @details `display()` is useful when the table-output from functions,
#'   which is usually printed as formatted text-table to console, should
#'   be formatted for pretty table-rendering in markdown documents, or if
#'   knitted from rmarkdown to PDF or Word files.
#'
#' @examples
#' data(iris)
#' corr <- correlation(iris)
#' display(corr)
#'
#' s <- summary(corr)
#' display(s)
#' @export
display.easycormatrix <- function(
  object,
  format = "markdown",
  digits = 2,
  p_digits = 3,
  stars = TRUE,
  include_significance = NULL,
  ...
) {
  if (format == "markdown") {
    print_md(
      x = object,
      digits = digits,
      p_digits = p_digits,
      stars = stars,
      include_significance = include_significance,
      ...
    )
  } else {
    print_html(
      x = object,
      digits = digits,
      p_digits = p_digits,
      stars = stars,
      include_significance = include_significance,
      ...
    )
  }
}


#' @export
display.easycorrelation <- function(
  object,
  format = "markdown",
  digits = 2,
  p_digits = 3,
  stars = TRUE,
  ...
) {
  if (format == "markdown") {
    print_md(
      x = object,
      digits = digits,
      p_digits = p_digits,
      stars = stars,
      ...
    )
  } else {
    print_html(
      x = object,
      digits = digits,
      p_digits = p_digits,
      stars = stars,
      ...
    )
  }
}
