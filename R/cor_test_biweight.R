#' @keywords internal
.cor_test_biweight <- function(data, x, y, ci = 0.95, ...) {
  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)


  # https://github.com/easystats/correlation/issues/13
  u <- (var_x - median(var_x)) / (9 * mad(var_x, constant = 1))
  v <- (var_y - median(var_y)) / (9 * mad(var_y, constant = 1))

  I_x <- ifelse((1 - abs(u)) > 0, 1, 0)
  I_y <- ifelse((1 - abs(v)) > 0, 1, 0)

  w_x <- I_x * (1 - u^2)^2
  w_y <- I_y * (1 - v^2)^2


  denominator_x <- sqrt(sum(((var_x - median(var_x)) * w_x)^2))
  x_curly <- ((var_x - median(var_x)) * w_x) / denominator_x

  denominator_y <- sqrt(sum(((var_y - median(var_y)) * w_y)^2))
  y_curly <- ((var_y - median(var_y)) * w_y) / denominator_y

  r <- sum(x_curly * y_curly)

  p <- cor_to_p(r, n = nrow(data))
  ci_vals <- cor_to_ci(r, n = nrow(data), ci = ci)

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    r = r,
    t = p$statistic,
    df = length(var_x) - 2,
    p = p$p,
    CI_low = ci_vals$CI_low,
    CI_high = ci_vals$CI_high,
    Method = "Biweight",
    stringsAsFactors = FALSE
  )
}
