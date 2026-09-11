# Bayesian Kendall's tau after van Doorn, Ly, Marsman & Wagenmakers (2018),
# "Bayesian Inference for Kendall's Rank Correlation Coefficient", The American
# Statistician 72(4), 303-308, doi:10.1080/00031305.2016.1264998. Equation
# numbers below refer to that paper.

#' @keywords internal
.cor_test_bayes_kendall <- function(
  data,
  x,
  y,
  ci = 0.95,
  bayesian_prior = "medium",
  bayesian_ci_method = "hdi",
  bayesian_test = c("pd", "rope", "bf"),
  ...
) {
  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)
  n <- length(var_x)
  alpha <- 1 / .bayes_kendall_scale(bayesian_prior)

  if (x == y) {
    # Same convention as the Bayesian Pearson path for a perfect correlation
    est <- list(
      tau = 1,
      CI_low = 1,
      CI_high = 1,
      pd = 1,
      ROPE_Percentage = 0,
      BF = Inf
    )
  } else {
    tau_obs <- stats::cor(var_x, var_y, method = "kendall")
    if (is.finite(tau_obs)) {
      est <- .bayes_kendall_posterior(
        n = n,
        tau = tau_obs,
        scale = bayesian_prior,
        ci = ci,
        ci_method = bayesian_ci_method
      )
    } else {
      est <- list(
        tau = NA_real_,
        CI_low = NA_real_,
        CI_high = NA_real_,
        pd = NA_real_,
        ROPE_Percentage = NA_real_,
        BF = NA_real_
      )
    }
  }

  out <- data.frame(
    Parameter1 = x,
    Parameter2 = y,
    tau = est$tau,
    CI = ci,
    CI_low = est$CI_low,
    CI_high = est$CI_high,
    pd = est$pd,
    ROPE_Percentage = est$ROPE_Percentage,
    Prior_Distribution = "beta",
    Prior_Location = alpha,
    Prior_Scale = alpha,
    BF = est$BF,
    Method = "Bayesian Kendall",
    stringsAsFactors = FALSE
  )

  # Keep the tests that were asked for, as the Pearson row does
  tests <- tolower(bayesian_test)
  if (!"pd" %in% tests) {
    out$pd <- NULL
  }
  if (!"rope" %in% tests) {
    out$ROPE_Percentage <- NULL
  }
  if (!"bf" %in% tests) {
    out$BF <- NA
  }
  out
}


#' Posterior of Kendall's tau (van Doorn et al. 2018)
#'
#' @param n Number of complete pairs.
#' @param tau Observed Kendall's tau (tau-b, as `stats::cor()` reports it).
#' @param scale Prior scale: one of the named scales of `bayesian_prior` or a
#'   positive number. The stretched-beta parameter is `alpha = 1 / scale`.
#' @param ci Interval level.
#' @param ci_method `"hdi"` or `"eti"`.
#' @return A list with `tau` (the posterior median), `CI_low`, `CI_high`,
#'   `pd`, `ROPE_Percentage` (posterior mass on `[-0.1, 0.1]`), `BF` (BF10 by
#'   the Savage-Dickey ratio), `alpha`, and `posterior`, the normalized
#'   posterior density as a function of tau.
#' @keywords internal
#' @noRd
.bayes_kendall_posterior <- function(
  n,
  tau,
  scale = "medium",
  ci = 0.95,
  ci_method = "hdi"
) {
  alpha <- 1 / .bayes_kendall_scale(scale)
  ci_method <- tolower(ci_method)
  if (!ci_method %in% c("hdi", "eti")) {
    insight::format_error(
      "`bayesian_ci_method` must be \"hdi\" or \"eti\" for the Bayesian Kendall correlation."
    )
  }

  # T*, eq. (6): the concordance count over sqrt(n(n - 1)(2n + 5)/18)
  t_star <- tau * (n * (n - 1) / 2) / sqrt(n * (n - 1) * (2 * n + 5) / 18)

  # Prior on tau, eq. (9): pi * 2^(-2 alpha) / B(alpha, alpha) * cos(pi tau / 2)^(2 alpha - 1)
  log_prior <- function(t) {
    log_cos <- if (alpha == 0.5) 0 else (2 * alpha - 1) * log(cos(pi * t / 2))
    log(pi) - 2 * alpha * log(2) - lbeta(alpha, alpha) + log_cos
  }
  # Likelihood, eq. (11): T* ~ N(1.5 tau sqrt(n), 1)
  log_lik <- function(t) {
    stats::dnorm(t_star, mean = 1.5 * t * sqrt(n), sd = 1, log = TRUE)
  }
  log_post <- function(t) log_lik(t) + log_prior(t)

  # Integrate a kernel scaled by the posterior mode so peaked posteriors
  # (large n, |tau| near 1) stay in floating-point range; every integral is
  # split at the mode so the peak is an endpoint of the quadrature.
  mode <- stats::optimize(function(t) -log_post(t), c(-1, 1), tol = 1e-10)
  mode <- mode$minimum
  lp_max <- log_post(mode)
  kernel <- function(t) exp(log_post(t) - lp_max)
  int <- function(a, b) {
    if (b <= a) {
      return(0)
    }
    stats::integrate(
      kernel,
      a,
      b,
      rel.tol = 1e-10,
      abs.tol = 0,
      subdivisions = 500L
    )$value
  }
  norm <- int(-1, mode) + int(mode, 1)
  cdf <- function(t) {
    if (t <= mode) int(-1, t) / norm else 1 - int(t, 1) / norm
  }
  quantile <- function(p) {
    stats::uniroot(
      function(t) cdf(t) - p,
      c(-1, 1),
      tol = 1e-10,
      maxiter = 1000L
    )$root
  }

  # BF10 = p(tau = 0) / p(tau = 0 | data), the reciprocal of eq. (13)
  bf <- exp(log(norm) + lp_max - log_lik(0))

  p_below_zero <- cdf(0)
  pd <- max(p_below_zero, 1 - p_below_zero)
  rope <- cdf(0.1) - cdf(-0.1)

  median <- quantile(0.5)
  if (ci_method == "eti") {
    ci_low <- quantile((1 - ci) / 2)
    ci_high <- quantile((1 + ci) / 2)
  } else {
    # Highest-density interval: the interval of mass `ci` with the smallest
    # width, searched over its lower end (the posterior is unimodal)
    width <- function(a) quantile(cdf(a) + ci) - a
    a_max <- quantile(1 - ci)
    ci_low <- stats::optimize(width, c(-1, a_max), tol = 1e-8)$minimum
    ci_high <- quantile(cdf(ci_low) + ci)
  }

  list(
    tau = median,
    CI_low = ci_low,
    CI_high = ci_high,
    pd = pd,
    ROPE_Percentage = rope,
    BF = bf,
    alpha = alpha,
    posterior = function(t) kernel(t) / norm
  )
}


#' @keywords internal
.bayes_kendall_scale <- function(scale) {
  named <- c(
    medium.narrow = 1 / sqrt(27),
    medium = 1 / 3,
    wide = 1 / sqrt(3),
    ultrawide = 1
  )
  if (is.character(scale) && length(scale) == 1 && scale %in% names(named)) {
    return(unname(named[scale]))
  }
  if (
    is.numeric(scale) && length(scale) == 1 && is.finite(scale) && scale > 0
  ) {
    return(scale)
  }
  insight::format_error(
    "`bayesian_prior` must be one of \"medium.narrow\", \"medium\", \"wide\", \"ultrawide\", or a positive number."
  )
}
