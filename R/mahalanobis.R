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
#' @examples
#' distance_mahalanobis(data=iris[,1:4])
#'
#' @export
distance_mahalanobis <- function(data, ci = 0.95, iterations=1000, robust=FALSE, ...) {
  if (robust) {
    Ms <- matrix(data=NA, nrow=iterations, ncol=nrow(data))
    for(i in 1:iterations){
      # Draw random numbers from 1:n with replacement
      x <- sample(1:nrow(data), nrow(data), replace = TRUE)
      # Resample data
      dat <- data[x,]
      # Calculating the Mahalanobis distance for each actual observation using resampled data
      m <- stats::mahalanobis(data, center = colMeans(dat), cov = stats::cov(dat))
      Ms[i,] <- m
    }
    # Get summary
    d <- bayestestR::describe_posterior(as.data.frame(Ms), centrality="median", ci=ci, test="pd")
    d <- d[c("Median", "CI_low", "CI_high")]
    names(d) <- c("Distance", "CI_low", "CI_high")

  } else {
    d <- stats::mahalanobis(data, center = colMeans(data), cov = stats::cov(data))
    d <- data.frame(Distance = d)
    # TODO: get ci
    # stats::pchisq(p = 1 - ci, df = ncol(data))
  }
  d
}
