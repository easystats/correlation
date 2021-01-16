#' @importFrom insight format_table
#' @export
format.easycorrelation <- function(x, digits = 2, stars = TRUE, ...) {
  insight::format_table(x, stars = stars, digits = digits)
}


#' @importFrom insight export_table
#' @importFrom parameters format_p_adjust
#' @export
print.easycorrelation <- function(x, digits = 2, stars = TRUE, ...) {
  formatted_table <- format(x, digits = digits, stars = stars)

  table_caption <- attributes(x)$method
  if (!is.null(table_caption)) {
    table_caption <- c(paste0("# Correlation table (", table_caption, "-method)"), "blue")
  }

  # footer
  footer <- attributes(x)$p_adjust
  if (!is.null(footer)) {
    footer <- paste0("\np-value adjustment method: ", parameters::format_p_adjust(footer))
    if (!is.null(x$n_Obs)) {
      footer <- list(footer, paste0("\nObservations: ", unique(x$n_Obs)))
    }
  }
  formatted_table$Method <- NULL
  formatted_table$n_Obs <- NULL

  cat(insight::export_table(formatted_table, format = "text", caption = table_caption, footer = footer))
  invisible(x)
}
