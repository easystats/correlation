#' @importFrom stats complete.cases
#' @importFrom utils capture.output
#' @keywords internal
.cor_test_polychoric <- function(data, x, y, ci = 0.95, ...) {
  insight::check_if_installed("psych", "for 'tetrachronic' correlations")

  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  # valid matrix check
  if (!is.factor(var_x) & !is.factor(var_y)) {
    stop("Polychoric correlations can only be ran on ordinal factors.")
  }


  if (!is.factor(var_x) | !is.factor(var_y)) {
    insight::check_if_installed("polycor", "for 'polyserial' correlations")

    r <- polycor::polyserial(
      x = if (is.factor(var_x)) as.numeric(var_y) else as.numeric(var_x),
      y = if (is.factor(var_x)) as.numeric(var_x) else as.numeric(var_y)
    )
    method <- "Polyserial"
  } else {
    # Reconstruct dataframe
    dat <- data.frame(as.numeric(var_x), as.numeric(var_y))
    names(dat) <- c(x, y)
    junk <- utils::capture.output(r <- psych::polychoric(dat)$rho[2, 1])
    method <- "Polychoric"
  }

  # t-value approximation
  p <- cor_to_p(r, n = length(var_x))
  ci_vals <- cor_to_ci(r, n = length(var_x), ci = ci)

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    t = p$statistic,
    df_error = length(var_x) - 2,
    p = p$p,
    CI_low = ci_vals$CI_low,
    CI_high = ci_vals$CI_high,
    Method = method,
    stringsAsFactors = FALSE
  )
}
