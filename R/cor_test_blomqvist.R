#' @keywords internal
.cor_test_blomqvist <- function(data, x, y, ci = 0.95, ...) {
  insight::check_if_installed("wdm", "for 'blomqvist' correlations")

  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  r <- wdm::wdm(var_x, var_y, method = "blomqvist")

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
    Method = "Blomqvist",
    stringsAsFactors = FALSE
  )
}
