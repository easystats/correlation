#' @keywords internal
.cor_test_hoeffding <- function(data, x, y, ci = 0.95, ...) {
  insight::check_if_installed("Hmisc", "for 'hoeffding' correlations")

  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  rez <- Hmisc::hoeffd(var_x, var_y)

  r <- rez$D[2, 1]
  p <- rez$P[2, 1]

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    r = r,
    t = NA,
    df_error = length(var_x) - 2,
    p = p,
    CI_low = NA,
    CI_high = NA,
    Method = "Hoeffding",
    stringsAsFactors = FALSE
  )
}
