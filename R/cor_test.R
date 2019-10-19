#' Correlation test
#'
#' This function perfoms a correlation test between two variables.
#'
#' @param data A dataframe.
#' @param x,y Names of two variables present in the data.
#' @param ci Confidence/Credible Interval level. If "default", then 0.95 for Frequentist and 0.89 for Bayesian (see documentation in the \pkg{bayestestR} package).
#' @param method A character string indicating which correlation coefficient is to be used for the test. One of "pearson" (default), "kendall", or "spearman", "polychoric", "tetrachoric", or "biweight". Setting "auto" will attempt at selecting the most relevant method (polychoric when ordinal factors involved, tetrachoric when dichotomous factors involved, and pearson otherwise).
#' @param bayesian If TRUE, will run a Bayesian correlation. Note that for "full" Bayesian partial correlations, you will also need to set \code{partial_bayesian} to \code{TRUE}. Otherwise, you will obtain pseudo-Bayesian partial correlations (i.e., Bayesian correlation based on frequentist partialization).
#' @param partial Can be TRUE or "semi" for partial and semi-partial correlations, respectively. This only works for Frequentist correlations.
#' @inheritParams partialize
#' @param partial_random,partial_bayesian See arguments of \code{\link{partialize}}.
#' @param bayesian_prior For the prior argument, several named values are recognized: "medium.narrow", "medium", "wide", and "ultrawide". These correspond to scale values of 1/sqrt(27), 1/3, 1/sqrt(3) and 1, respectively. See the \code{BayesFactor::correlationBF} function.
#' @param bayesian_ci_method,bayesian_test See arguments in \code{\link[=parameters]{model_parameters}} for \code{BayesFactor} tests.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' data <- iris
#'
#' cor_test(iris, "Petal.Length", "Petal.Width")
#' cor_test(iris, "Petal.Length", "Petal.Width", method = "spearman")
#' cor_test(iris, "Petal.Length", "Petal.Width", method = "kendall")
#' cor_test(iris, "Petal.Length", "Petal.Width", method = "biweight")
#' cor_test(iris, "Petal.Length", "Petal.Width", bayesian = TRUE)
#'
#' # Tetrachoric
#' data$Sepal.Width_binary <- ifelse(data$Sepal.Width > 3, 1, 0)
#' data$Petal.Width_binary <- ifelse(data$Petal.Width > 1.2, 1, 0)
#' cor_test(data, "Sepal.Width_binary", "Petal.Width_binary", method = "tetrachoric")
#'
#' # When one variable is continuous and the other binary, will run 'biserial' correlation
#' cor_test(data, "Sepal.Width", "Petal.Width_binary", method = "tetrachoric")
#'
#' # Polychoric
#' data$Petal.Width_ordinal <- as.factor(round(data$Petal.Width))
#' data$Sepal.Length_ordinal <- as.factor(round(data$Sepal.Length))
#' cor_test(data, "Petal.Width_ordinal", "Sepal.Length_ordinal", method = "polychoric")
#'
#' # When one variable is continuous, will run 'polyserial' correlation
#' cor_test(data, "Sepal.Width", "Sepal.Length_ordinal", method = "polychoric")
#' @export
cor_test <- function(data, x, y, method = "pearson", ci = "default", bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), partial = FALSE, include_factors = TRUE, partial_random = FALSE, partial_bayesian = FALSE, ...) {

  # Partial
  if(partial){
    data <- partialize(data, x, y, include_factors = include_factors, random = partial_random, bayesian = partial_bayesian)
  }

  # Frequentist
  if (bayesian == FALSE) {
    if (ci == "default") ci <- 0.95

    if(method == "auto") method <- .cor_test_findtype(data, x, y)

    if (tolower(method) %in% c("tetra", "tetrachoric")) {
      out <- .cor_test_tetrachoric(data, x, y, ci = ci, ...)
    } else if (tolower(method) %in% c("poly", "polychoric")) {
      out <- .cor_test_polychoric(data, x, y, ci = ci, ...)
    } else if (tolower(method) %in% c("biweight")) {
      out <- .cor_test_biweight(data, x, y, ci = ci, ...)
    } else {
      out <- .cor_test_freq(data, x, y, ci = ci, method = method, ...)
    }

    # Bayesian
  } else {
    if (ci == "default") ci <- 0.89

    if (method %in% c("tetra", "tetrachoric")) {
      stop("Tetrachoric Bayesian correlations are not supported yet.")
    } else {
      out <- .cor_test_bayes(data, x, y, ci = ci, bayesian_prior = bayesian_prior, bayesian_ci_method = bayesian_ci_method, bayesian_test = bayesian_test, ...)
    }
  }

  class(out) <- unique(c("easycorrelation", "parameters_model", class(out)))
  out
}










#' @importFrom stats cor.test complete.cases
#' @keywords internal
.cor_test_freq <- function(data, x, y, ci = 0.95, method = "pearson", ...) {
  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(data[[x]], data[[y]])]
  var_y <- var_y[complete.cases(data[[x]], data[[y]])]

  rez <- cor.test(var_x, var_y, conf.level = ci, method = match.arg(method, c("pearson", "kendall", "spearman"), several.ok = FALSE), alternative = "two.sided")

  params <- parameters::model_parameters(rez)
  params$Parameter1 <- x
  params$Parameter2 <- y

  if (x == y) {
    if ("t" %in% names(params)) params$t <- Inf
    if ("z" %in% names(params)) params$z <- Inf
    if ("S" %in% names(params)) params$S <- Inf
  }

  params
}






#' @importFrom stats complete.cases rnorm
#' @importFrom utils install.packages
#' @keywords internal
.cor_test_bayes <- function(data, x, y, ci = 0.89, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), ...) {
  if (!requireNamespace("BayesFactor")) {
    stop("This function needs `BayesFactor` to be installed. Please install by running `install.packages('BayesFactor')`.")
  }

  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(var_x, var_y)]
  var_y <- var_y[complete.cases(var_x, var_y)]

  if (x == y) {
    # Avoid error in the case of perfect correlation
    rez <- BayesFactor::correlationBF(rnorm(1000), rnorm(1000), rscale = bayesian_prior)
    params <- parameters::model_parameters(rez, ci_method = bayesian_ci_method, test = bayesian_test, rope_range = c(-0.1, 0.1), rope_ci = 1, ...)
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
    params <- parameters::model_parameters(rez, ci_method = bayesian_ci_method, test = bayesian_test, rope_range = c(-0.1, 0.1), rope_ci = 1, ...)
  }

  # Rename coef
  if (sum(names(params) %in% c("Median", "Mean", "MAP")) == 1) {
    names(params)[names(params) %in% c("Median", "Mean", "MAP")] <- "rho"
  }

  params <- params[names(params) != "Parameter"]
  params$Parameter1 <- x
  params$Parameter2 <- y
  params[unique(c("Parameter1", "Parameter2", names(params)))]
}




#' @importFrom stats complete.cases
#' @importFrom utils capture.output
#' @keywords internal
.cor_test_tetrachoric <- function(data, x, y, ci = 0.95, ...) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package `psych` required for tetrachoric correlations. Please install it by running `install.packages('psych').", call. = FALSE)
  }

  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(var_x, var_y)]
  var_y <- var_y[complete.cases(var_x, var_y)]

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

  stats <- cor_to_p(r, n = nrow(data), ci = ci)

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    t = stats$statistic,
    df = length(var_x) - 2,
    p = stats$p,
    CI_low = stats$ci_low,
    CI_high = stats$ci_high,
    Method = method,
    stringsAsFactors = FALSE
  )
}



#' @importFrom stats complete.cases
#' @importFrom utils capture.output
#' @keywords internal
.cor_test_polychoric <- function(data, x, y, ci = 0.95, ...) {
  if (!requireNamespace("psych", quietly = TRUE)) {
    stop("Package `psych` required for tetrachoric correlations. Please install it by running `install.packages('psych').", call. = FALSE)
  }

  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(var_x, var_y)]
  var_y <- var_y[complete.cases(var_x, var_y)]

  # Sanity check
  if (!is.factor(var_x) & !is.factor(var_y)) {
    stop("Polychoric correlations can only be ran on ordinal factors.")
  }



  if (!is.factor(var_x) | !is.factor(var_y)) {
    if (!requireNamespace("polycor", quietly = TRUE)) {
      stop("Package `polycor` required for polyserial correlations. Please install it by running `install.packages('polycor').", call. = FALSE)
    }
    r <- polycor::polyserial(
      x = if (is.factor(var_x)) as.numeric(var_y) else as.numeric(var_x),
      y = if (is.factor(var_x)) as.numeric(var_x) else as.numeric(var_y)
    )
    method <- "Polyserial"
  } else {
    # Reconstruct dataframe
    dat <- data.frame(as.numeric(var_x), as.numeric(var_y))
    names(dat) <- c(x, y)
    junk <- capture.output(r <- psych::polychoric(dat)$rho[2, 1])
    method <- "Polychoric"
  }


  stats <- cor_to_p(r, n = nrow(data), ci = ci)

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    t = stats$statistic,
    df = length(var_x) - 2,
    p = stats$p,
    CI_low = stats$ci_low,
    CI_high = stats$ci_high,
    Method = method,
    stringsAsFactors = FALSE
  )
}




#' @keywords internal
.cor_test_biweight <- function(data, x, y, ci = 0.95, ...) {

  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(var_x, var_y)]
  var_y <- var_y[complete.cases(var_x, var_y)]


  # https://github.com/easystats/correlation/issues/13
  u <- (var_x - median(var_x)) / (9 * mad(var_x, constant = 1))
  v <- (var_y - median(var_y)) / (9 * mad(var_y, constant = 1))

  I_x <- ifelse((1 - abs(u)) > 0, 1, 0)
  I_y <- ifelse((1 - abs(v)) > 0, 1, 0)

  w_x <- I_x * (1 - u^2)^2
  w_y <- I_y * (1 - v^2)^2


  denominator_x <- sqrt(sum(((var_x - median(var_x)) * w_x)^2 ))
  x_curly <- ((var_x - median(var_x)) * w_x) / denominator_x

  denominator_y <- sqrt(sum(((var_y - median(var_y) ) * w_y)^2))
  y_curly <- ((var_y - median(var_y)) * w_y) / denominator_y

  r <- sum(x_curly * y_curly)

  stats <- cor_to_p(r, n = nrow(data), ci = ci)

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    r = r,
    t = stats$statistic,
    df = length(var_x) - 2,
    p = stats$p,
    CI_low = stats$ci_low,
    CI_high = stats$ci_high,
    Method = "Biweight",
    stringsAsFactors = FALSE
  )
}




#' @keywords internal
.cor_test_findtype <- function(data, x, y){
  if (length(unique(data[[x]])) == 2 | length(unique(data[[y]])) == 2) {
    current_method <- "tetrachoric"
  } else if (is.factor(data[x]) | is.factor(data[y])) {
    current_method <- "polychoric"
  } else {
    current_method <- "pearson"
  }
  current_method
}

