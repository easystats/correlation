#' @importFrom stats complete.cases
#' @importFrom utils capture.output
#' @keywords internal
.cor_test_tetrachoric <- function(data, x, y, ci = 0.95, ...) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package `psych` required for tetrachoric correlations. Please install it by running `install.packages('psych').", call. = FALSE)
  }

  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  # Sanity check
  if (length(unique(var_x)) > 2 & length(unique(var_y)) > 2) {
    stop("Tetrachoric correlations can only be ran on dichotomous data.")
  }

  # Reconstruct dataframe
  dat <- data.frame(var_x, var_y)
  names(dat) <- c(x, y)

  if (length(unique(var_x)) > 2 | length(unique(var_y)) > 2) {
    junk <- capture.output(r <- psych::biserial(
      x = dat[sapply(dat, function(x) length(unique(x)) > 2)],
      y = dat[sapply(dat, function(x) !length(unique(x)) > 2)]
    )[1])
    method <- "Biserial"
  } else {
    junk <- capture.output(r <- psych::tetrachoric(dat)$rho[2, 1])
    method <- "Tetrachoric"
  }

  p <- cor_to_p(r, n = nrow(data))
  ci_vals <- cor_to_ci(r, n = nrow(data), ci = ci)

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    t = p$statistic,
    df = length(var_x) - 2,
    p = p$p,
    CI_low = ci_vals$CI_low,
    CI_high = ci_vals$CI_high,
    Method = method,
    stringsAsFactors = FALSE
  )
}
