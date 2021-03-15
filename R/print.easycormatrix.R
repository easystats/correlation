#' @importFrom insight format_p format_pd format_bf format_value export_table
#' @export
format.easycormatrix <- function(x, digits = 2, stars = NULL, include_significance = NULL, p_digits = NULL, ...) {

  # Round and format values
  nums <- sapply(as.data.frame(x), is.numeric)
  x[, nums] <- sapply(as.data.frame(x)[, nums], insight::format_value, digits = digits)


  # Find attributes
  attri <- attributes(x)

  # Retrieve arguments from attributes (or assign default)
  stars <- .retrieve_arg_from_attr(attri, stars, default = TRUE)
  include_significance <- .retrieve_arg_from_attr(attri, include_significance, default = FALSE)
  p_digits <- .retrieve_arg_from_attr(attri, p_digits, default = "apa")

  # Deduct if stars only
  stars_only <- FALSE
  if(include_significance == FALSE && stars == TRUE) {
    stars_only <- TRUE
  }


  # Significance
  type <- names(attri)[names(attri) %in% c("BF", "pd", "p")][1]
  sig <- attri[[type]]

  if (!is.null(sig)) {
    if (type == "p") {
      sig[, nums] <- sapply(sig[, nums], insight::format_p, stars = stars, digits = p_digits, stars_only = stars_only)
    } else if (type == "pd") {
      sig[, nums] <- sapply(sig[, nums], insight::format_pd, stars = stars, stars_only = stars_only)
    } else if (type == "BF") {
      sig[, nums] <- sapply(sig[, nums], insight::format_bf, stars = stars, stars_only = stars_only)
    }
    if(stars_only == FALSE) {
      sig[, nums] <- sapply(sig[, nums], function(x) ifelse(x != "", paste0(" (", x, ")"), ""))
    }

    if(include_significance | stars) x[, nums] <- paste0(as.matrix(as.data.frame(x)[, nums]), as.matrix(sig[, nums]))

  }

  as.data.frame(x)
}





#' @importFrom insight export_table
#' @export
print.easycormatrix <- function(x, digits = 2, stars = NULL, include_significance = NULL, p_digits = NULL, ...) {
  formatted_table <- format(x, digits = digits, stars = stars, include_significance = include_significance, p_digits = p_digits, ...)

  table_caption <- NULL
  if (!is.null(attributes(x)$method)) {
    table_caption <- c(paste0("# Correlation Matrix (", unique(attributes(x)$method), "-method)"), "blue")
  }

  formatted_table$Method <- NULL
  formatted_table$n_Obs <- NULL

  cat(insight::export_table(formatted_table,
    format = "text",
    caption = table_caption,
    footer = .print_easycorrelation_add_footer(x)
  ))
  invisible(x)
}





#' @keywords internal
.retrieve_arg_from_attr <- function(attributes, arg, default) {
  arg_name <- deparse(substitute(arg))
  if (is.null(arg)) {
    if (arg_name %in% names(attributes)) {
      arg <- attributes[[arg_name]]
    } else {
      arg <- default # That's the real default
    }
  }
  arg
}