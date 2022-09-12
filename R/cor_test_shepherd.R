#' @keywords internal
.cor_test_shepherd <- function(data, x, y, ci = 0.95, bayesian = FALSE, ...) {
  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  d <- .robust_bootstrap_mahalanobis(cbind(var_x, var_y))
  not_outliers <- d < 6

  if (bayesian) {
    data <- data[not_outliers, ]
    data[c(x, y)] <- datawizard::ranktransform(data[c(x, y)], sign = TRUE, method = "average")
    out <- .cor_test_bayes(data, x, y, ci = ci)
  } else {
    out <- .cor_test_freq(data[not_outliers, ], x, y, ci = ci, method = "spearman")
  }
  out$Method <- "Shepherd's Pi"
  out
}


# Utils -------------------------------------------------------------------

#' @keywords internal
.robust_bootstrap_mahalanobis <- function(data, iterations = 1000) {
  Ms <- replicate(n = iterations, {
    # Draw random numbers from 1:n with replacement
    idx <- sample(nrow(data), replace = TRUE)
    # Resample data
    dat <- data[idx, ]
    # Calculating the Mahalanobis distance for each actual observation using resampled data
    stats::mahalanobis(data, center = colMeans(dat), cov = stats::cov(dat))
  })

  apply(Ms, 1, median)
}


# # Mahalanobis distance and confidence interval (CI)
# #
# # The Mahalanobis distance (in squared units) measures the distance in
# # multivariate space taking into account the covariance structure of the data.
# # Because a few extreme outliers can skew the covariance estimate, the
# # bootstrapped version is considered as more robust.
# #
# # @inheritParams correlation
# # @param iterations The number of draws to simulate/bootstrap (when
# #   `robust` is `TRUE`).
# # @param robust If `TRUE`, will run a bootstrapped version of the function
# #   with i iterations.
# #
# # @references
# #
# # - Schwarzkopf, D. S., De Haas, B., & Rees, G. (2012). Better ways to
# #   improve standards in brain-behavior correlation analysis. Frontiers in
# #   human neuroscience, 6, 200.
# #
# #
# # @return Description of the Mahalanobis distance.
# #
# # @examples
# # library(correlation)
# #
# # distance_mahalanobis(iris[, 1:4])
# # distance_mahalanobis(iris[, 1:4], robust = FALSE)
# # @export
# distance_mahalanobis <- function(data,
#                                  ci = 0.95,
#                                  iterations = 1000,
#                                  robust = TRUE,
#                                  ...) {
#   if (robust) {
#     Ms <- matrix(data = NA, nrow = iterations, ncol = nrow(data))
#     for (i in 1:iterations) {
#       # Draw random numbers from 1:n with replacement
#       x <- sample(seq_len(nrow(data)), nrow(data), replace = TRUE)
#       # Resample data
#       dat <- data[x, ]
#       # Calculating the Mahalanobis distance for each actual observation using resampled data
#       m <- stats::mahalanobis(data, center = colMeans(dat), cov = stats::cov(dat))
#       Ms[i, ] <- m
#     }
#     # Get summary
#     d <- bayestestR::describe_posterior(
#       as.data.frame(Ms),
#       centrality = "median",
#       ci = ci,
#       test = "pd"
#     )
#     d <- as.data.frame(d[c("Median", "CI_low", "CI_high")])
#     rownames(d) <- NULL
#     names(d) <- c("Distance", "CI_low", "CI_high")
#   } else {
#     d <- stats::mahalanobis(data, center = colMeans(data), cov = stats::cov(data))
#     d <- data.frame(Distance = d)
#     # TODO: get ci
#     # stats::pchisq(p = 1 - ci, df = ncol(data))
#   }
#   d
# }