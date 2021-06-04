#' @keywords internal
.cor_test_somers <- function(data, x, y, ci = 0.95, ...) {
  insight::check_if_installed("Hmisc", "for 'somers' correlations")

  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  rez <- Hmisc::somers2(var_y, var_x)
  r <- rez["Dxy"]

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    Dxy = r,
    t = NA,
    df_error = length(var_x) - 2,
    p = NA,
    CI_low = NA,
    CI_high = NA,
    Method = "Somers",
    stringsAsFactors = FALSE
  )
}
