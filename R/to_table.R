#' Export tables into different output formats
#'
#' Export tables (i.e. data frame) into different output formats
#'
#' @param x An object returned by \code{\link[=correlation]{correlation()}}
#'   or its summary.
#' @param format String, indicating the output format. Currently, only
#'   \code{"markdown"} is supported.
#' @param digits To do...
#' @param stars To do...
#' @param ... Currently not used.
#'
#' @return A character vector. If \code{format = "markdown"}, the return value
#'   will be a character vector in markdown-table format.
#'
#' @details \code{to_table()} is useful when the table-output from functions,
#'   which is usually printed as formatted text-table to console, should
#'   be formatted for pretty table-rendering in markdown documents, or if
#'   knitted from rmarkdown to PDF or Word files.
#'
#' @examples
#' data(iris)
#' corr <- correlation(iris)
#' to_table(corr)
#'
#' s <- summary(corr)
#' to_table(s)
#' @importFrom insight format_table
#' @export
to_table.easycormatrix <- function(x, format = "markdown", digits = 2, stars = TRUE, ...) {
  table_caption <- attributes(x)$method
  if (!is.null(table_caption)) {
    table_caption <- paste0("Correlation Matrix (", table_caption, "-method)")
  }
  insight::format_table(format(x, digits = digits, stars = stars), format = format, caption = table_caption, align = "firstleft")
}


#' @importFrom parameters parameters_table
#' @export
to_table.easycorrelation <- function(x, format = "markdown", digits = 2, stars = TRUE, ...) {
  table_caption <- attributes(x)$method
  if (!is.null(table_caption)) {
    table_caption <- paste0("Correlation Matrix (", table_caption, "-method)")
  }
  formatted_table <- parameters::parameters_table(x, pretty_names = TRUE, digits = digits, stars = stars, ci_width = NULL, ci_brackets = c("(", ")"))
  insight::format_table(formatted_table, format = format, caption = table_caption, align = "firstleft")
}

# Reexports models ------------------------

#' @importFrom insight to_table
#' @export
insight::to_table
