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
