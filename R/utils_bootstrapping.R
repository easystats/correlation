#' @keywords internal
.bootstrap_data <- function(data, n) {
  # generate bootstrap resamples
  strap <- replicate(n, .resample(data), simplify = FALSE)

  # add resample ID, may be used for other functions
  for (i in seq_len(length(strap))) strap[[i]]$Resample_id <- i

  # return as list variable
  data.frame(bootstraps = I(strap))
}




#' @keywords internal
.resample <- function(data) {
  structure(
    class = "correlation_resample",
    list(
      data = data,
      id = sample(nrow(data), size = nrow(data), replace = TRUE)
    )
  )
}




#' @importFrom stats qt sd quantile na.omit
#' @keywords internal
.bootstrapped_ci <- function(data, select = NULL, method = c("normal", "quantile"), ci.lvl = .95) {
  # match arguments
  method <- match.arg(method)

  if (is.null(select)) {
    .dat <- data
  } else {
    .dat <- data[select]
  }

  # compute confidence intervals for all values
  .transform_boot_result(lapply(.dat, function(x) {
    # check if method should be based on t-distribution of
    # bootstrap values or quantiles
    if (method == "normal") {
      # get bootstrap standard error
      bootse <- stats::qt((1 + ci.lvl) / 2, df = length(stats::na.omit(x)) - 1) * stats::sd(x, na.rm = T)
      # lower and upper confidence interval
      ci <- mean(x, na.rm = T) + c(-bootse, bootse)
    } else {
      # CI based on quantiles of bootstrapped values
      ci <- stats::quantile(x, probs = c((1 - ci.lvl) / 2, (1 + ci.lvl) / 2))
    }
    # give proper names
    names(ci) <- c("CI_low", "CI_high")
    ci
  }))
}




#' @keywords internal
.transform_boot_result <- function(result) {
  as.data.frame(t(as.data.frame(result)))
}




#' @keywords internal
as.data.frame.correlation_resample <- function(x, ...) {
  x$data[x$id, , drop = FALSE]
}
