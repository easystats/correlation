#' Smooth a non-positive definite correlation matrix to make it positive definite
#'
#' Make correlations positive definite using `psych::cor.smooth`. Note that this modifies the correlation values, and does **not** recompute the remaining indices (p-values, confidence intervals, etc.). Thus, once you used `cor_smooth()`, any other indices becomes irrelevant.
#'
#' @param x A correlation matrix.
#' @param method Smoothing method. Can be `psych` (will use `psych::cor.smooth()`), `hj` (Jorjani et al., 2003) or `lrs` (Schaeffer, 2014). For the two last, will use `mbend::bend()` (check its documentation for details).
#' @param verbose Set to `FALSE` to silence the function.
#' @param tol The minimum eigen values to be considered as acceptable.
#' @param ... Other arguments to be passed to or from other functions.
#'
#' @examples
#' set.seed(1)
#' data <- cbind(mtcars,
#'               mtcars * matrix(rnorm(32 * 11, sd = 0.05), ncol=11),
#'               mtcars * matrix(rnorm(32 * 11, sd = 0.05), ncol=11),
#'               mtcars * -matrix(rnorm(32 * 11, sd = 0.01), ncol=11))
#' x <- correlation(data)
#' is.positive_definite(x)
#'
#' smoothed <- cor_smooth(x)
#' @export
cor_smooth <- function(x, method = "psych", verbose = TRUE, ...) {
  UseMethod("cor_smooth")
}


#' @export
cor_smooth.easycorrelation <- function(x, method = "psych", verbose = TRUE, tol = .Machine$double.eps, ...) {
  m <- cor_smooth(as.matrix(x), method = method, verbose = verbose, tol = tol, ...)
  estim <- names(x)[names(x) %in% c("r", "rho", "tau", "D")][1]

  for(param1 in row.names(m)) {
    for(param2 in colnames(m)) {
      if(nrow(x[x$Parameter1 == param1 & x$Parameter2 == param2, ]) == 0) next
      # Print changes
      if(verbose) {
        val1 <- x[x$Parameter1 == param1 & x$Parameter2 == param2, estim]
        val2 <- m[param1, param2]
        if(val1 == val2) {
          insight::print_color(paste0(param1,
                                      " - ",
                                      param2,
                                      ": no change (",
                                      insight::format_value(val1),
                                      ")\n"), "green")
        } else {
          insight::print_color(paste0(param1,
                                      " - ",
                                      param2,
                                      ": ",
                                      insight::format_value(val1),
                                      " -> ",
                                      insight::format_value(val2),
                                      "\n"), "red")
        }
      }
      x[x$Parameter1 == param1 & x$Parameter2 == param2, estim] <- m[param1, param2]
    }
  }
  x
}


#' @export
cor_smooth.matrix <- function(x, method = "psych", verbose = TRUE, tol = .Machine$double.eps, ...) {

  method <- match.arg(method, choices = c("psych", "hj", "lrs"))

  # Already positive definite
  if(is.positive_definite(x, tol = tol, ...) == TRUE) {
    if(verbose) message("Matrix is positive definite, smoothing was not needed.")
    return(x)
  }

  if(method == "psych") {
    insight::check_if_installed("psych")
    x <- suppressWarnings(psych::cor.smooth(x, eig.tol = tol, ...))
  } else {
    out <- try(suppressMessages(mbend::bend(x, method = method, ...)), silent = TRUE)
    if(inherits(out, as.character("try-error"))) return(x)
    x <- out$bent
  }
  x
}


# Utils -------------------------------------------------------------------

#' @rdname cor_smooth
#' @export
is.positive_definite <- function(x, tol = .Machine$double.eps, ...) {
  UseMethod("is.positive_definite")
}


#' @export
is.positive_definite.matrix <- function(x, tol = .Machine$double.eps, ...) {
  eigens <- try(eigen(x), silent = TRUE)

  # Sanity checks
  if(inherits(eigens, as.character("try-error"))) {
    stop('There is something seriously wrong with the correlation matrix, as some of the eigen values are NA.')
  }
  # Find out
  if(min(eigens$values) >= tol) {
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

