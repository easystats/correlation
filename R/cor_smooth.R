#' Smooth a non-positive definite correlation matrix to make it positive definite
#'
#' Make correlations positive definite using `psych::cor.smooth`. If smoothing
#' is done, inferential statistics (*p*-values, confidence intervals, etc.) are
#' removed, as they are no longer valid.
#'
#' @param x A correlation matrix.
#' @param method Smoothing method. Can be `psych` (will use
#'   `psych::cor.smooth()`), `hj` (Jorjani et al., 2003) or `lrs` (Schaeffer,
#'   2014). For the two last, will use `mbend::bend()` (check its documentation
#'   for details).
#' @param verbose Set to `FALSE` to silence the function.
#' @param tol The minimum eigenvalue to be considered as acceptable.
#' @param ... Other arguments to be passed to or from other functions.
#'
#' @examplesIf requireNamespace("psych", quietly = TRUE)
#' set.seed(123)
#' data <- as.matrix(mtcars)
#' # Make missing data so pairwise correlation matrix is non-positive definite
#' data[sample(seq_len(352), size = 60)] <- NA
#' data <- as.data.frame(data)
#' x <- correlation(data)
#' is.positive_definite(x)
#'
#' smoothed <- cor_smooth(x)
#' @export
cor_smooth <- function(x, method = "psych", verbose = TRUE, ...) {
  UseMethod("cor_smooth")
}


#' @export
cor_smooth.easycorrelation <- function(x,
                                       method = "psych",
                                       verbose = TRUE,
                                       tol = 10^-12,
                                       ...) {
  m <- cor_smooth(as.matrix(x), method = method, verbose = verbose, tol = tol, ...)

  if (isTRUE(attributes(m)$smoothed)) {
    estim <- names(x)[names(x) %in% c("r", "rho", "tau", "D")][1]

    for (param1 in row.names(m)) {
      for (param2 in colnames(m)) {
        if (nrow(x[x$Parameter1 == param1 & x$Parameter2 == param2, ]) == 0) next
        # Print changes
        if (verbose) {
          val1 <- x[x$Parameter1 == param1 & x$Parameter2 == param2, estim]
          val2 <- m[param1, param2]
          if (round(val1 - val2, digits = 2) == 0) {
            insight::print_color(paste0(
              param1,
              " - ",
              param2,
              ": no change (",
              insight::format_value(val1),
              ")\n"
            ), "green")
          } else {
            insight::print_color(paste0(
              param1,
              " - ",
              param2,
              ": ",
              insight::format_value(val1),
              " -> ",
              insight::format_value(val2),
              "\n"
            ), "red")
          }
          cat("\n")
        }
        x[x$Parameter1 == param1 & x$Parameter2 == param2, estim] <- m[param1, param2]
      }
    }

    atts <- attributes(x)
    x <- x[, c("Parameter1", "Parameter2", "r", "Method", "n_Obs")]
    atts$names <- names(x)
    atts$smoothed <- TRUE
    attributes(x) <- atts
    x
  } else {
    x
  }
}


#' @export
cor_smooth.matrix <- function(x,
                              method = "psych",
                              verbose = TRUE,
                              tol = 10^-12,
                              ...) {
  method <- match.arg(method, choices = c("psych", "hj", "lrs"))

  # Already positive definite
  if (is.positive_definite(x, tol = tol, ...) == TRUE) {
    if (verbose) message("Matrix is positive definite, smoothing was not needed.")
    return(x)
  }

  if (method == "psych") {
    insight::check_if_installed("psych")
    x <- suppressWarnings(psych::cor.smooth(x, eig.tol = tol, ...))
  } else {
    out <- try(suppressMessages(mbend::bend(x, method = method, ...)), silent = TRUE)
    if (inherits(out, as.character("try-error"))) {
      return(x)
    }
    x <- out$bent
  }

  attr(x, "smoothed") <- TRUE
  x
}


# Utils -------------------------------------------------------------------

#' @rdname cor_smooth
#' @export
is.positive_definite <- function(x, tol = 10^-12, ...) {
  UseMethod("is.positive_definite")
}

#' @rdname cor_smooth
#' @export
is_positive_definite <- is.positive_definite

#' @export
is.positive_definite.matrix <- function(x, tol = 10^-12, ...) {
  eigens <- try(eigen(x), silent = TRUE)

  # validation checks
  if (inherits(eigens, as.character("try-error"))) {
    stop(insight::format_message(
      "There is something seriously wrong with the correlation matrix, as some of the eigen values are NA."
    ), call. = FALSE)
  }

  # Find out
  if (min(eigens$values) >= tol) {
    out <- TRUE
  } else {
    out <- FALSE
  }

  out
}

#' @export
is.positive_definite.easycorrelation <- function(x, ...) {
  is.positive_definite(as.matrix(x, ...), ...)
}
