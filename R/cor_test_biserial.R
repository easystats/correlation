#' @keywords internal
.cor_test_biserial <- function(data, x, y, ci = 0.95, method = "biserial", ...) {
  # valid matrix
  if (.vartype(data[[x]])$is_binary && !.vartype(data[[y]])$is_binary) {
    binary <- x
    continuous <- y
  } else if (.vartype(data[[y]])$is_binary && !.vartype(data[[x]])$is_binary) {
    binary <- y
    continuous <- x
  } else {
    insight::format_error(
      "Biserial and point-biserial correlations can only be applied for one dichotomous and one continuous variables."
    )
  }

  # Rescale to 0-1
  if (.vartype(data[[binary]])$is_factor || .vartype(data[[binary]])$is_character) {
    data[[binary]] <- as.numeric(as.factor(data[[binary]]))
  }

  data[[binary]] <- as.vector(
    (data[[binary]] - min(data[[binary]], na.rm = TRUE)) /
      (diff(range(data[[binary]], na.rm = TRUE), na.rm = TRUE))
  )

  # Get biserial or point-biserial correlation
  if (method == "biserial") {
    out <- .cor_test_biserial_biserial(data, x, y, continuous, binary, ci)
  } else {
    out <- .cor_test_biserial_pointbiserial(data, x, y, continuous, binary, ci, ...)
  }

  out
}


#' @keywords internal
.cor_test_biserial_pointbiserial <- function(data, x, y, continuous, binary, ci, ...) {
  out <- .cor_test_freq(data, continuous, binary, ci = ci, method = "pearson", ...)
  names(out)[names(out) == "r"] <- "rho"
  out$Parameter1 <- x
  out$Parameter2 <- y
  out$Method <- "Point-biserial"

  out
}


#' @keywords internal
.cor_test_biserial_biserial <- function(data, x, y, continuous, binary, ci) {
  var_x <- .complete_variable_x(data, continuous, binary)
  var_y <- .complete_variable_y(data, continuous, binary)


  m1 <- mean(var_x[var_y == 1])
  m0 <- mean(var_x[var_y == 0])
  quan <- mean(var_y)
  p <- 1 - quan
  zp <- stats::dnorm(stats::qnorm(quan))

  r <- (((m1 - m0) * (p * quan / zp)) / stats::sd(var_x))

  p <- cor_to_p(r, n = length(var_x))
  ci_vals <- cor_to_ci(r, n = length(var_x), ci = ci)

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    t = p$statistic,
    df_error = length(var_y) - 2,
    p = p$p,
    CI_low = ci_vals$CI_low,
    CI_high = ci_vals$CI_high,
    Method = "Biserial",
    stringsAsFactors = FALSE
  )
}
