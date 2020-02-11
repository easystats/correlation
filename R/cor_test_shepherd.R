#' @importFrom effectsize ranktransform
#' @keywords internal
.cor_test_shepherd <- function(data, x, y, ci = 0.95, bayesian = FALSE, ...) {
  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  d <- distance_mahalanobis(cbind(var_x, var_y))
  not_outliers <- d$Distance < 6

  if (bayesian) {
    data <- data[not_outliers, ]
    data[c(x, y)] <- effectsize::ranktransform(data[c(x, y)], sign = TRUE, method = "average")
    out <- .cor_test_bayes(data, x, y, ci = ci)
  } else {
    out <- .cor_test_freq(data[not_outliers, ], x, y, ci = ci, method = "spearman")
  }
  out$Method <- "Shepherd's Pi"
  out
}
