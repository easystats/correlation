
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

.print_md_html_easycorrelation <- function(x,
                                           digits = NULL,
                                           stars = NULL,
                                           p_digits = NULL,
                                           format = "markdown",
                                           ...) {

  formatted_table <- format(
    x,
    digits = digits,
    stars = stars,
    p_digits = p_digits,
    ci_width = NULL,
    ci_brackets = c("(", ")"),
    format = format,
    ...
  )

  insight::export_table(
    formatted_table,
    format = format,
    align = "firstleft",
    ...
  )
}

#' @export
print_md.easycorrelation <- function(x,
                                     digits = NULL,
                                     p_digits = NULL,
                                     stars = NULL,
                                     ...) {
  .print_md_html_easycorrelation(
    x,
    digits = digits,
    p_digits = p_digits,
    stars = stars,
    format = "markdown",
    ...
  )
}


#' @export
print_html.easycorrelation <- function(x,
                                       digits = NULL,
                                       p_digits = NULL,
                                       stars = NULL,
                                       ...) {
  .print_md_html_easycorrelation(
    x,
    digits = digits,
    p_digits = p_digits,
    stars = stars,
    format = "html",
    ...
  )
}



.print_md_html_easycormatrix <- function(x,
                                         digits = NULL,
                                         p_digits = NULL,
                                         stars = NULL,
                                         include_significance = NULL,
                                         format = "markdown",
                                         ...) {

  formatted_table <- format(
    x,
    digits = digits,
    p_digits = p_digits,
    stars = stars,
    include_significance = include_significance,
    ci_width = NULL,
    ci_brackets = c("(", ")"),
    format = format,
    ...
  )

  insight::export_table(
    formatted_table,
    format = format,
    align = "firstleft",
    ...
  )
}

#' @export
print_md.easycormatrix <- function(x,
                                   digits = NULL,
                                   p_digits = NULL,
                                   stars = NULL,
                                   include_significance = NULL,
                                   ...) {
  .print_md_html_easycormatrix(
    x,
    digits = digits,
    p_digits = p_digits,
    stars = stars,
    include_significance = include_significance,
    format = "markdown",
    ...
  )
}


#' @export
print_html.easycormatrix <- function(x,
                                     digits = NULL,
                                     p_digits = NULL,
                                     stars = NULL,
                                     include_significance = NULL,
                                     ...) {
  .print_md_html_easycormatrix(
    x,
    digits = digits,
    p_digits = p_digits,
    stars = stars,
    include_significance = include_significance,
    format = "html",
    ...
  )
}




# Reexports functions ------------------------

#' @importFrom insight print_md
#' @export
insight::print_md


#' @importFrom insight print_html
#' @export
insight::print_html
