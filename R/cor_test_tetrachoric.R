#' @keywords internal
.cor_test_tetrachoric <- function(data, x, y, ci = 0.95, ...) {
  insight::check_if_installed("psych", "for 'tetrachronic' correlations")

  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  # valid matrix check
  if (length(unique(var_x)) > 2 && length(unique(var_y)) > 2) {
    stop("Tetrachoric correlations can only be ran on dichotomous data.")
  }

  # Reconstruct dataframe
  dat <- data.frame(var_x, var_y)
  names(dat) <- c(x, y)

  junk <- utils::capture.output(r <- psych::tetrachoric(dat)$rho[2, 1])

  p <- cor_to_p(r, n = nrow(data))
  ci_vals <- cor_to_ci(r, n = nrow(data), ci = ci)

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    t = p$statistic,
    df_error = length(var_x) - 2,
    p = p$p,
    CI_low = ci_vals$CI_low,
    CI_high = ci_vals$CI_high,
    Method = "Tetrachoric",
    stringsAsFactors = FALSE
  )
}
