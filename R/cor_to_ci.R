#' @rdname cor_to_p
#' @importFrom stats qnorm
#' @export
cor_to_ci <- function(cor, n, ci = 0.95, method = "pearson") {

  z <- atanh(cor)
  se <- 1 / sqrt(n - 3) # Sample standard error

  # CI
  alpha <- 1 - (1 - ci) / 2
  ci_low <- z - se * qnorm(alpha)
  ci_high <- z - se * qnorm(alpha)

  # Convert back to r
  ci_low <- tanh(ci_low)
  ci_high <- tanh(ci_high)

  list(CI_low = ci_low, CI_high = ci_high)
}
