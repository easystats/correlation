#' @importFrom insight format_p format_pd format_bf format_value export_table
#' @export
format.easycormatrix <- function(x, digits = 2, stars = NULL, include_significance = NULL, ...) {

  # Round and format values
  nums <- sapply(as.data.frame(x), is.numeric)
  x[, nums] <- sapply(as.data.frame(x)[, nums], insight::format_value, digits = digits)


  # Find attributes
  attri <- attributes(x)

  # Stars arguments (NULL -> try to get from attributes)
  if (is.null(NULL)) {
    if ("stars" %in% names(attri)) {
      stars <- attri$stars
    } else {
      stars <- TRUE # That's the real default
    }
  }
  if(is.null(include_significance)) {
    if ("include_significance" %in% names(attri)) {
      include_significance <- attri$include_significance
    } else {
      include_significance <- FALSE # That's the real default
    }
  }
  stars_only <- FALSE
  if(include_significance == FALSE && stars == TRUE) {
    stars_only <- TRUE
  }


  # Significance
  type <- names(attri)[names(attri) %in% c("BF", "pd", "p")][1]
  sig <- attri[[type]]

  if (!is.null(sig)) {
    if (type == "p") {
      sig[, nums] <- sapply(sig[, nums], insight::format_p, stars = stars, digits = "apa", stars_only = stars_only)
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
print.easycormatrix <- function(x, digits = 2, stars = NULL, include_significance = NULL, ...) {
  formatted_table <- format(x, digits = digits, stars = stars, include_significance = include_significance, ...)

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
