#' @rdname cor_to_p
#' @param correction Only used if method is 'spearman' or 'kendall'. Can be 'fieller', 'bw' or 'none'. Bonett and Wright (2000) claim their correction ('bw') performs better, though the Bishara and Hittner (2017) paper favours the Fieller correction. Both are generally very similar.
#' @importFrom stats qnorm
#' @export
cor_to_ci <- function(cor, n, ci = 0.95, method = "pearson", correction = "fieller") {
  method <- match.arg(tolower(method), c("pearson", "kendall", "spearman"), several.ok = FALSE)

  if (method == "kendall") {
    out <- .cor_to_ci_kendall(cor, n, ci = ci, correction=correction)
  } else if (method == "spearman") {
    out <- .cor_to_ci_spearman(cor, n, ci = ci, correction=correction)
  } else {
    out <- .cor_to_ci_pearson(cor, n, ci = ci)
  }

  out
}





# Kendall -----------------------------------------------------------------
.cor_to_ci_kendall <- function(cor, n, ci = 0.95, correction = "fieller", ...) {
  # by @tsbaguley (https://rpubs.com/seriousstats/616206)

  if (correction == "fieller") {
    tau.se <- (0.437 / (n - 4))^0.5
  } else {
    tau.se <- 1 / (n - 3)^0.5
  }

  moe <- qnorm(1 - (1 - ci) / 2) * tau.se
  zu <- atanh(cor) + moe
  zl <- atanh(cor) - moe

  # Convert back to r
  ci_low <- tanh(zl)
  ci_high <- tanh(zu)

  list(CI_low = ci_low, CI_high = ci_high)
}


# Spearman -----------------------------------------------------------------
.cor_to_ci_spearman <- function(cor, n, ci = 0.95, correction = "fieller", ...) {
  # by @tsbaguley (https://rpubs.com/seriousstats/616206)

  if (correction == "fieller") {
    zrs.se <- (1.06 / (n - 3))^0.5
  } else if (correction == "bw") {
    zrs.se <- ((1 + (cor^2) / 2) / (n - 3))^0.5
  } else {
    zrs.se <- 1 / (n - 3)^0.5
  }

  moe <- qnorm(1 - (1 - ci) / 2) * zrs.se

  zu <- atanh(cor) + moe
  zl <- atanh(cor) - moe

  # Convert back to r
  ci_low <- tanh(zl)
  ci_high <- tanh(zu)

  list(CI_low = ci_low, CI_high = ci_high)
}


# Pearson -----------------------------------------------------------------
.cor_to_ci_pearson <- function(cor, n, ci = 0.95, ...) {
  z <- atanh(cor)
  se <- 1 / sqrt(n - 3) # Sample standard error

  # CI
  alpha <- 1 - (1 - ci) / 2
  ci_low <- z - se * stats::qnorm(alpha)
  ci_high <- z + se * stats::qnorm(alpha)

  # Convert back to r
  ci_low <- tanh(ci_low)
  ci_high <- tanh(ci_high)

  list(CI_low = ci_low, CI_high = ci_high)
}
