#' @keywords internal
.cor_test_gaussian <- function(data, x, y, ci = 0.95, ...) {
  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  var_x <- qnorm(rank(var_x) / (length(var_x) + 1))
  var_y <- qnorm(rank(var_y) / (length(var_y) + 1))

  out <- .cor_test_base(x, y, var_x, var_y, ci = ci, method = "pearson", ...)
  out$Method <- "Gaussian rank"
  out
}
