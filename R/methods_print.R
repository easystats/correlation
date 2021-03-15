
# Console -----------------------------------------------------------------


#' @importFrom insight export_table
#' @export
print.easycorrelation <- function(x, ...) {
  formatted_table <- format(x, ...)
  cat(insight::export_table(formatted_table, format = "text"))
  invisible(x)
}

#' @export
print.easycormatrix <- print.easycorrelation



# MD and HTML --------------------------------------------------------------

.print_md_html_easycorrelation <- function(x, format = "markdown", ...) {
  formatted_table <- format(x, ci_width = NULL, ci_brackets = c("(", ")"), ...)
  insight::export_table(
    formatted_table,
    format = format,
    align = "firstleft",
    ...
  )
}

#' @export
print_md.easycorrelation <- function(x, ...) {
  .print_md_html_easycorrelation(x, format = "markdown", ...)
}


#' @export
print_html.easycorrelation <- function(x, ...) {
  .print_md_html_easycorrelation(x, format = "html", ...)
}

.print_md_html_easycormatrix <- function(x, format = "markdown", ...) {
  formatted_table <- format(x, ...)
  insight::export_table(
    formatted_table,
    format = format,
    align = "firstleft",
    ...
  )
}

#' @export
print_md.easycormatrix <- function(x, ...) {
  .print_md_html_easycormatrix(x, format = "markdown", ...)
}


#' @export
print_html.easycormatrix <- function(x, ...) {
  .print_md_html_easycormatrix(x, format = "html", ...)
}





# Reexports functions ------------------------

#' @importFrom insight print_md
#' @export
insight::print_md


#' @importFrom insight print_html
#' @export
insight::print_html
