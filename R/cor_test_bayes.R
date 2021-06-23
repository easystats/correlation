#' @importFrom stats complete.cases rnorm qnorm
#' @importFrom utils install.packages
#' @keywords internal
.cor_test_bayes <- function(data,
                            x,
                            y,
                            ci = 0.95,
                            method = "pearson",
                            bayesian_prior = "medium",
                            bayesian_ci_method = "hdi",
                            bayesian_test = c("pd", "rope", "bf"),
                            ...) {
  insight::check_if_installed("BayesFactor")

  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  if (tolower(method) %in% c("spearman", "spear", "s")) {
    var_x <- datawizard::ranktransform(var_x, sign = TRUE, method = "average")
    var_y <- datawizard::ranktransform(var_y, sign = TRUE, method = "average")
    method <- "Bayesian Spearman"
  } else if (tolower(method) %in% c("gaussian")) {
    var_x <- stats::qnorm(rank(var_x) / (length(var_x) + 1))
    var_y <- stats::qnorm(rank(var_y) / (length(var_y) + 1))
    method <- "Bayesian Gaussian rank"
  } else {
    method <- "Bayesian Pearson"
  }

  out <- .cor_test_bayes_base(
    x,
    y,
    var_x,
    var_y,
    ci = ci,
    bayesian_prior = bayesian_prior,
    bayesian_ci_method = bayesian_ci_method,
    bayesian_test = bayesian_test,
    ...
  )

  # Add method
  out$Method <- method
  out
}


#' @keywords internal
.cor_test_bayes_base <- function(x,
                                 y,
                                 var_x,
                                 var_y,
                                 ci = 0.95,
                                 bayesian_prior = "medium",
                                 bayesian_ci_method = "hdi",
                                 bayesian_test = c("pd", "rope", "bf"),
                                 method = "pearson",
                                 ...) {
  insight::check_if_installed("BayesFactor")

  if (x == y) {
    # Avoid error in the case of perfect correlation
    rez <- BayesFactor::correlationBF(rnorm(1000), rnorm(1000), rscale = bayesian_prior)
    params <- parameters::model_parameters(
      rez,
      dispersion = FALSE,
      ci_method = bayesian_ci_method,
      test = bayesian_test,
      rope_range = c(-0.1, 0.1),
      rope_ci = 1,
      ...
    )
    if ("Median" %in% names(params)) params$Median <- 1
    if ("Mean" %in% names(params)) params$Mean <- 1
    if ("MAP" %in% names(params)) params$MAP <- 1
    if ("SD" %in% names(params)) params$SD <- 0
    if ("MAD" %in% names(params)) params$MAD <- 0
    if ("CI_low" %in% names(params)) params$CI_low <- 1
    if ("CI_high" %in% names(params)) params$CI_high <- 1
    if ("pd" %in% names(params)) params$pd <- 1
    if ("ROPE_Percentage" %in% names(params)) params$ROPE_Percentage <- 0
    if ("BF" %in% names(params)) params$BF <- Inf
  } else {
    rez <- BayesFactor::correlationBF(var_x, var_y, rscale = bayesian_prior)
    params <- parameters::model_parameters(
      rez,
      dispersion = FALSE,
      ci_method = bayesian_ci_method,
      test = bayesian_test,
      rope_range = c(-0.1, 0.1),
      rope_ci = 1,
      ...
    )
  }

  # Rename coef
  if (sum(names(params) %in% c("Median", "Mean", "MAP")) == 1) {
    names(params)[names(params) %in% c("Median", "Mean", "MAP")] <- "rho"
  }

  # Remove useless columns
  params[names(params) %in% c("Effects", "Component")] <- NULL

  # Prepare output
  params <- params[names(params) != "Parameter"]
  params$Parameter1 <- x
  params$Parameter2 <- y
  params[unique(c("Parameter1", "Parameter2", names(params)))]
}
