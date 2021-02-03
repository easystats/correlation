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


  formatted_table$Method <- NULL
  formatted_table$n_Obs <- NULL

  cat(insight::export_table(formatted_table,
                            format = "text",
                            caption = table_caption,
                            footer = .print_easycorrelation_add_footer(x)))
  invisible(x)
}



#' @keywords internal
.print_easycorrelation_add_footer <- function(x){

  footer <- ""

  # P-adjust
  if (attributes(x)$bayesian == FALSE) {
    footer <- paste0(footer,
                     "\np-value adjustment method: ",
                     parameters::format_p_adjust(attributes(x)$p_adjust))
  }

  # N-obs
  if (!is.null(x$n_Obs)) {
    if(length(unique(x$n_Obs)) == 1){
      nobs <- unique(x$n_Obs)
    } else{
      nobs <- paste0(min(x$n_Obs), "-", max(x$n_Obs))
    }
    footer <- paste0(footer, "\nObservations: ", nobs)
  }
  footer
}