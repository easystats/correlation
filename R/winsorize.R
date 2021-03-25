#' Winsorize data
#'
#' Winsorizing or winsorization is the transformation of statistics by limiting
#' extreme values in the statistical data to reduce the effect of possibly
#' spurious outliers. The distribution of many statistics can be heavily
#' influenced by outliers. A typical strategy is to set all outliers (values
#' beyond a certain threshold) to a specified percentile of the data; for
#' example, a 90\% winsorization would see all data below the 5th percentile set
#' to the 5th percentile, and data above the 95th percentile set to the 95th
#' percentile. Winsorized estimators are usually more robust to outliers than
#' their more standard forms.
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
winsorize.factor <- function(data, ...) {
  data
}

#' @export
winsorize.character <- winsorize.factor

#' @export
winsorize.logical <- winsorize.factor

#' @export
winsorize.data.frame <- function(data, threshold = 0.2, verbose = TRUE, ...) {
  sapply(data, winsorize, threshold = threshold, verbose = verbose)
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

  y <- sort(data)
  n <- length(data)
  ibot <- floor(threshold * n) + 1
  itop <- length(data) - ibot + 1
  xbot <- y[ibot]
  xtop <- y[itop]
  winval <- ifelse(data <= xbot, xbot, data)
  ifelse(winval >= xtop, xtop, winval)
}
