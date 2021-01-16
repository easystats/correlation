#' @importFrom parameters format_p_adjust
#' @importFrom insight export_table parameters_table
#' @rdname display.easycormatrix
#' @export
print_md.easycormatrix <- function(x, digits = 2, stars = TRUE, ...) {
  .print_md_html_easycormatrix(x, digits, stars, format = "markdown")
}


#' @export
print_html.easycormatrix <- function(x, digits = 2, stars = TRUE, ...) {
  .print_md_html_easycormatrix(x, digits, stars, format = "html")
}


.print_md_html_easycormatrix <- function(x, digits, stars, format = "markdown") {
  table_caption <- attributes(x)$method
  if (!is.null(table_caption)) {
    table_caption <- paste0("Correlation Matrix (", table_caption, "-method)")
  }
  footer <- attributes(x)$p_adjust
  if (!is.null(footer)) {
    footer <- paste0("p-value adjustment method: ", parameters::format_p_adjust(footer))
    if (!is.null(x$n_Obs)) {
      footer <- list(footer, paste0("Observations: ", unique(x$n_Obs)))
    }
  }
  x$Method <- NULL
  x$n_Obs <- NULL
  insight::export_table(format(x, digits = digits, stars = stars), format = format, caption = table_caption, footer = footer, align = "firstleft")
}






#' @export
print_md.easycorrelation <- function(x, digits = 2, stars = TRUE, ...) {
  .print_md_html_easycorrelation(x, digits, stars, format = "markdown")
}


#' @export
print_html.easycorrelation <- function(x, digits = 2, stars = TRUE, ...) {
  .print_md_html_easycorrelation(x, digits, stars, format = "html")
}


.print_md_html_easycorrelation <- function(x, digits, stars, format = "markdown") {
  # caption
  table_caption <- attributes(x)$method
  if (!is.null(table_caption)) {
    table_caption <- paste0("Correlation Matrix (", table_caption, "-method)")
  }

  # footer
  footer <- attributes(x)$p_adjust
  if (!is.null(footer)) {
    footer <- paste0("p-value adjustment method: ", parameters::format_p_adjust(footer))
    if (!is.null(x$n_Obs)) {
      footer <- list(footer, paste0("Observations: ", unique(x$n_Obs)))
    }
  }
  x$Method <- NULL
  x$n_Obs <- NULL

  # final table
  formatted_table <- insight::parameters_table(x, pretty_names = TRUE, digits = digits, stars = stars, ci_width = NULL, ci_brackets = c("(", ")"))
  insight::export_table(formatted_table, format = format, caption = table_caption, align = "firstleft", footer = footer)
}






# Reexports models ------------------------

#' @importFrom insight print_md
#' @export
insight::print_md


#' @importFrom insight print_html
#' @export
insight::print_html
