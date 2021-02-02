#' @keywords internal
.cor_test_gamma <- function(data, x, y, ci = 0.95, ...) {
  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  # Get r value
  Rx <- outer(var_x, var_x, function(u, v) sign(u - v))
  Ry <- outer(var_y, var_y, function(u, v) sign(u - v))
  S1 <- Rx * Ry
  r <- sum(S1) / sum(abs(S1))

  # t-value approximation
  p <- cor_to_p(r, n = length(var_x))
  ci_vals <- cor_to_ci(r, n = length(var_x), ci = ci)


  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    r = r,
    t = p$statistic,
    df_error = length(var_x) - 2,
    p = p$p,
    CI_low = ci_vals$CI_low,
    CI_high = ci_vals$CI_high,
    Method = "Gamma",
    stringsAsFactors = FALSE
  )
}
