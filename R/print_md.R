#' @importFrom insight export_table parameters_table
#' @rdname display.easycormatrix
#' @export
print_md.easycormatrix <- function(x, digits = 2, stars = TRUE, ...) {
  table_caption <- attributes(x)$method
  if (!is.null(table_caption)) {
    table_caption <- paste0("Correlation Matrix (", table_caption, "-method)")
  }
  insight::export_table(format(x, digits = digits, stars = stars), format = "markdown", caption = table_caption, align = "firstleft")
}

#' @export
print_md.easycorrelation <- function(x, digits = 2, stars = TRUE, ...) {
  # caption
  table_caption <- "Correlation Matrix"

  # footer
  if (!is.null(x$Method) && !is.null(x$n_Obs)) {
    footer <- paste0("Method: ", x$Method[1])
    if (length(unique(x$n_Obs)) == 1) {
      footer <- paste0(footer, " (", x$n_Obs[1], " observations)")
      x$n_Obs <- NULL
    }
    footer <- paste0("_", footer, "_")
  } else {
    footer <- NULL
  }
  x$Method <- NULL

  # final table
  formatted_table <- insight::parameters_table(x, pretty_names = TRUE, digits = digits, stars = stars, ci_width = NULL, ci_brackets = c("(", ")"))
  insight::export_table(formatted_table, format = "markdown", caption = table_caption, align = "firstleft", footer = footer)
}


# Reexports models ------------------------

#' @importFrom insight print_md
#' @export
insight::print_md
