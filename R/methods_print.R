# Console -----------------------------------------------------------------


#' @export
print.easycorrelation <- function(x, ...) {
  cat(insight::export_table(format(x, ...), ...))
  invisible(x)
}

#' @export
print.easycormatrix <- function(x, ...) {
  formatted <- format(x, ...)
  # If real matrix, print as matrix
  if (colnames(formatted)[1] == "Variables") {
    formatted$Variables <- NULL
    print(as.matrix(formatted), ...)
  } else {
    cat(insight::export_table(format(x, ...), ...))
  }
  invisible(x)
}


#' @export
print.easymatrixlist <- function(x, cols = "auto", ...) {
  if (cols == "auto") {
    cols <- c(names(x)[1], "n_Obs", "p")
  }

  cols <- cols[cols %in% names(x)]

  for (i in cols) {
    cat(" ", i, " ", "\n", rep("-", nchar(i) + 2), "\n", sep = "")
    print(x[[i]], ...)
    cat("\n")
  }
}

#' @export
print.grouped_easymatrixlist <- function(x, cols = "auto", ...) {
  for (i in names(x)) {
    cat(rep("=", nchar(i) + 2), "\n ", i, " ", "\n", rep("=", nchar(i) + 2), "\n\n", sep = "")
    print(x[[i]], ...)
    cat("\n")
  }
}

# MD and HTML --------------------------------------------------------------

.print_md_html_easycorrelation <- function(x,
                                           digits = NULL,
                                           p_digits = NULL,
                                           stars = NULL,
                                           format = "markdown",
                                           ...) {
  formatted_table <- format(
    x,
    digits = digits,
    p_digits = p_digits,
    stars = stars,
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


#' @rdname display.easycormatrix
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


#' @rdname display.easycormatrix
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


#' @rdname display.easycormatrix
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


#' @rdname display.easycormatrix
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
