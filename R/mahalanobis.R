#' Mahalanobis distance
#'
#' The Mahalanobis distance (in squared units) measures the distance in multivariate space taking into account the covariance structure of the data. Because a few extreme outliers can skew the covariance estimate, the bootstrapped version is considered as more robust.
#'
#' @inheritParams correlation
#' @param iterations The number of draws to simulate/bootstrap (when \code{robust} is \code{TRUE}).
#' @param robust If \code{TRUE}, will run a bootstrapped version of the function with i iterations.
#'
#' @references \itemize{
#'   \item Schwarzkopf, D. S., De Haas, B., & Rees, G. (2012). Better ways to improve standards in brain-behavior correlation analysis. Frontiers in human neuroscience, 6, 200.
#' }
#'
#' @export
distance_mahalanobis <- function(data, ci = 0.95, iterations=1000, robust=FALSE, ...) {
  if (robust) {
    # rez <- boot::boot(data = data, statistic = .distance_mahalanobis, R = iterations, sim="permutation")
    # bayestestR::point_estimate(as.data.frame(rez$t), centrality="all")
    stop("not available yet.")
  } else {
    d <- .distance_mahalanobis(data)
    # TODO: get ci
    # stats::pchisq(p = 1 - ci, df = ncol(data))
  }
  d
}





#' @keywords internal
.distance_mahalanobis <- function(data, indices = 1:nrow(data), ...) {
  dat <- data[indices, ] # allows boot to select sample
  row.names(dat) <- NULL
  stats::mahalanobis(dat, center = colMeans(dat), cov = stats::cov(dat))
}