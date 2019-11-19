#' @export
print.easycormatrix <- function(x, digits = 2, stars = TRUE, ...) {
  nums <- sapply(as.data.frame(x), is.numeric)

  # Significance
  p <- attributes(x)
  type <- names(p)[names(p) %in% c("BF", "pd", "p")][1]
  p <- p[[type]]

  if (!is.null(p)) {
    if (type == "p") {
      p[, nums] <- sapply(p[, nums], parameters::format_p, stars_only = TRUE)
    } else if (type == "pd") {
      p[, nums] <- sapply(p[, nums], parameters::format_pd, stars_only = TRUE)
    } else if (type == "BF") {
      p[, nums] <- sapply(p[, nums], parameters::format_bf, stars_only = TRUE)
    }

    # Round and eventually add stars
    x[, nums] <- sapply(as.data.frame(x)[, nums], insight::format_value, digits = digits)
    if (stars) {
      x[, nums] <- paste0(as.matrix(as.data.frame(x)[, nums]), as.matrix(p[, nums]))
    }
  } else {
    x[, nums] <- sapply(as.data.frame(x)[, nums], insight::format_value, digits = digits)
  }



  cat(insight::format_table(x))
  invisible(x)
}
