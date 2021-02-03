#' @importFrom stats cor.test
#' @importFrom parameters model_parameters
#' @keywords internal
.cor_test_freq <- function(data, x, y, ci = 0.95, method = "pearson", ...) {
  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  .cor_test_base(x, y, var_x, var_y, ci = ci, method = method, ...)
}


#' @keywords internal
.cor_test_base <- function(x, y, var_x, var_y, ci = 0.95, method = "pearson", ...) {
  method <- match.arg(tolower(method), c("pearson", "kendall", "spearman", "somers"), several.ok = FALSE)
  rez <- stats::cor.test(var_x, var_y, conf.level = ci, method = method, exact = FALSE, ...)

  # params <- parameters::model_parameters(rez)
  # this doubles performance according to computation time
  params <- .extract_corr_parameters(rez)

  params$Parameter1 <- x
  params$Parameter2 <- y

  if (x == y) {
    if ("t" %in% names(params)) params$t <- Inf
    if ("z" %in% names(params)) params$z <- Inf
    if ("S" %in% names(params)) params$S <- Inf
  }

  # Add CI for non-pearson correlations
  if (method %in% c("kendall", "spearman")) {
    rez_ci <- cor_to_ci(rez$estimate, n = length(var_x), ci = ci, method = method, ...)
    params$CI_low <- rez_ci$CI_low
    params$CI_high <- rez_ci$CI_high
  }

  # see ?cor.test: CI only in case of at least 4 complete pairs of observations
  if (!("CI_low" %in% names(params))) params$CI_low <- NA
  if (!("CI_high" %in% names(params))) params$CI_high <- NA

  params
}



.extract_corr_parameters <- function(model) {
  names <- unlist(strsplit(model$data.name, " and ", fixed = TRUE))
  out <- data.frame(
    "Parameter1" = names[1],
    "Parameter2" = names[2],
    stringsAsFactors = FALSE
  )

  if (model$method == "Pearson's Chi-squared test") {
    out$Chi2 <- model$statistic
    out$df_error <- model$parameter
    out$p <- model$p.value
    out$Method <- "Pearson"
  } else if (grepl("Pearson", model$method)) {
    out$r <- model$estimate
    out$t <- model$statistic
    out$df_error <- model$parameter
    out$p <- model$p.value
    out$CI_low <- model$conf.int[1]
    out$CI_high <- model$conf.int[2]
    out$Method <- "Pearson"
  } else if (grepl("Spearman", model$method)) {
    out$rho <- model$estimate
    out$S <- model$statistic
    out$df_error <- model$parameter
    out$p <- model$p.value
    out$Method <- "Spearman"
  } else {
    out$tau <- model$estimate
    out$z <- model$statistic
    out$df_error <- model$parameter
    out$p <- model$p.value
    out$Method <- "Kendall"
  }
  out
}
