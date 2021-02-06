#' Winsorize data
#'
#' Winsorization of data.
#'
#' @param data Dataframe or vector.
#' @param threshold The amount of winsorization.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @examples
#' library(correlation)
#'
#' winsorize(iris$Sepal.Length, threshold = 0.2)
#' winsorize(iris, threshold = 0.2)
#' @export
winsorize <- function(data, ...) {
  UseMethod("winsorize")
}


#' @export
winsorize.factor <- function(data, threshold = 0.2, ...) {
  stats::na.omit(data)
}

#' @export
winsorize.character <- winsorize.factor

#' @export
winsorize.logical <- winsorize.factor

#' @export
winsorize.data.frame <- function(data, threshold = 0.2, verbose = TRUE, ...) {
  sapply(stats::na.omit(data), winsorize, threshold = threshold, verbose = verbose)
}

#' @rdname winsorize
#' @export
winsorize.numeric <- function(data, threshold = 0.2, verbose = TRUE, ...) {
  if (threshold < 0 || threshold > 1) {
    if (isTRUE(verbose)) {
      warning("'threshold' for winsorization must be a scalar between 0 and 1. Did not winsorize data.", call. = FALSE)
    }
    return(data)
  }

  data <- stats::na.omit(data)
  y <- sort(data)
  n <- length(data)
  ibot <- floor(threshold * n) + 1
  itop <- length(data) - ibot + 1
  xbot <- y[ibot]
  xtop <- y[itop]
  winval <- ifelse(data <= xbot, xbot, data)
  ifelse(winval >= xtop, xtop, winval)
}
