#' @keywords internal
.cor_test_biserial <- function(data, x, y, ci = 0.95, method = "biserial", ...) {

  # Sanity
  if (.vartype(data[[x]])$is_binary & .vartype(data[[y]])$is_binary == FALSE) {
    binary <- x
    continuous <- y
  } else if (.vartype(data[[y]])$is_binary & .vartype(data[[x]])$is_binary == FALSE) {
    binary <- y
    continuous <- x
  } else {
    stop("Biserial and point-biserial correlations can only be applied for one dichotomous and one continuous variables.")
  }

  # Rescale to 0-1
  if (.vartype(data[[binary]])$is_factor | .vartype(data[[binary]])$is_character) {
    data[[binary]] <- as.numeric(as.factor(data[[binary]]))
  }
  data[[binary]] <- as.vector((data[[binary]] - min(data[[binary]], na.rm = TRUE)) / diff(range(data[[binary]], na.rm = TRUE), na.rm = TRUE))

  # Get biserial correlation
  if (method == "biserial") {
    out <- .cor_test_biserial_biserial(data, x, y, continuous, binary, ci)

    # Get point-biserial correlation
  } else {
    out <- .cor_test_freq(data, continuous, binary, ci = ci, method = "pearson", ...)
    names(out)[names(out) == "r"] <- "rho"
    out$Parameter1 <- x
    out$Parameter2 <- y
    out$Method <- "Point-biserial"
  }

  out
}














#' @keywords internal
.cor_test_biserial_biserial <- function(data, x, y, continuous, binary, ci) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package `psych` required for biserial correlations. Please install it by running `install.packages('psych').", call. = FALSE)
  }

  var_x <- .complete_variable_x(data, continuous, binary)
  var_y <- .complete_variable_y(data, continuous, binary)


  m1 <- mean(var_x[var_y == 1])
  m0 <- mean(var_x[var_y == 0])
  q <- mean(var_y)
  p <- 1 - q
  zp <- stats::dnorm(stats::qnorm(q))

  r <- (((m1 - m0) * (p * q / zp)) / sd(var_x))

  p <- cor_to_p(r, n = length(var_x))
  ci_vals <- cor_to_ci(r, n = length(var_x), ci = ci)

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    t = p$statistic,
    df = length(var_y) - 2,
    p = p$p,
    CI_low = ci_vals$CI_low,
    CI_high = ci_vals$CI_high,
    Method = "Biserial",
    stringsAsFactors = FALSE
  )
}
