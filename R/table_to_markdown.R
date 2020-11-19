#' @rdname to_table.easycormatrix
#' @export
table_to_markdown.easycormatrix <- function(x, digits = 2, stars = TRUE, ...) {
  to_table(x = x, format = "markdown", digits = digits, stars = stars, ...)
}

#' @export
table_to_markdown.easycorrelation <- table_to_markdown.easycormatrix


# Reexports models ------------------------

#' @importFrom insight table_to_markdown
#' @export
insight::table_to_markdown
