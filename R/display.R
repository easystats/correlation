#' @title Export tables into different output formats
#' @name display.easycormatrix
#'
#' @description Export tables (i.e. data frame) into different output formats.
#'   \code{print_md()} is a alias for \code{display(format = "markdown")}.
#'
#' @param object,x An object returned by
#'   \code{\link[=correlation]{correlation()}} or its summary.
#' @param format String, indicating the output format. Currently, only
#'   \code{"markdown"} is supported.
#' @param digits,p_digits To do...
#' @param stars To do...
#' @param include_significance To do...
#' @param ... Currently not used.
#'
#' @return A character vector. If \code{format = "markdown"}, the return value
#'   will be a character vector in markdown-table format.
#'
#' @details \code{display()} is useful when the table-output from functions,
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
display.easycormatrix <- function(object,
                                  format = "markdown",
                                  digits = 2,
                                  p_digits = 3,
                                  stars = TRUE,
                                  include_significance = NULL,
                                  ...) {
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
display.easycorrelation <- function(object,
                                    format = "markdown",
                                    digits = 2,
                                    p_digits = 3,
                                    stars = TRUE,
                                    ...) {
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


# Reexports models ------------------------

#' @importFrom insight display
#' @export
insight::display
