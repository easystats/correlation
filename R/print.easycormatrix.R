#' @importFrom insight format_p format_pd format_bf format_value export_table
#' @export
format.easycormatrix <- function(x, digits = 2, stars = "default", ...) {
  nums <- sapply(as.data.frame(x), is.numeric)

  # Find attributes
  attri <- attributes(x)

  if (stars == "default"){
    if("stars" %in% names(attri)) {
      stars <- attri$stars
    } else {
      stars <- TRUE
    }
  }

  # Significance
  type <- names(attri)[names(attri) %in% c("BF", "pd", "p")][1]
  attri <- attri[[type]]

  if (!is.null(attri)) {
    if (type == "p") {
      attri[, nums] <- sapply(attri[, nums], insight::format_p, stars_only = TRUE)
    } else if (type == "pd") {
      attri[, nums] <- sapply(attri[, nums], insight::format_pd, stars_only = TRUE)
    } else if (type == "BF") {
      attri[, nums] <- sapply(attri[, nums], insight::format_bf, stars_only = TRUE)
    }

    # Round and eventually add stars
    x[, nums] <- sapply(as.data.frame(x)[, nums], insight::format_value, digits = digits)
    if (stars) {
      x[, nums] <- paste0(as.matrix(as.data.frame(x)[, nums]), as.matrix(attri[, nums]))
    }
  } else {
    x[, nums] <- sapply(as.data.frame(x)[, nums], insight::format_value, digits = digits)
  }

  as.data.frame(x)
}


#' @importFrom insight export_table
#' @export
print.easycormatrix <- function(x, digits = 2, stars = "default", ...) {
  formatted_table <- format(x, digits = digits, stars = stars)

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
