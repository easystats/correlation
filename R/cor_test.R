#' Correlation test
#'
#' This function perfoms a correlation test between two variables.
#'
#' @inheritParams correlation
#' @param x Name of a variable.
#' @param y Name of a variable.
#'
#' @examples
#' data <- iris
#'
#' cor_test(iris, "Petal.Length", "Petal.Width")
#' cor_test(iris, "Petal.Length", "Petal.Width", method = "spearman")
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
cor_test <- function(data, x, y, ci = "default", method = "pearson", bayesian = FALSE, ...) {

  # Frequentist
  if (bayesian == FALSE) {
    if (ci == "default") ci <- 0.95

    if (tolower(method) %in% c("tetra", "tetrachoric")) {
      out <- .cor_test_tetrachoric(data, x, y, ...)
    } else if (tolower(method) %in% c("poly", "polychoric")) {
      out <- .cor_test_polychoric(data, x, y, ...)
    } else {
      out <- .cor_test_freq(data, x, y, ci = ci, method = method, ...)
    }

    # Bayesian
  } else {
    if (ci == "default") ci <- 0.89

    if (method %in% c("tetra", "tetrachoric")) {
      stop("Tetrachoric Bayesian correlations are not supported yet.")
    } else {
      out <- .cor_test_bayes(data, x, y, ci = ci, ...)
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
.cor_test_bayes <- function(data, x, y, ci = 0.89, prior = "medium", ...) {
  if (!requireNamespace("BayesFactor")) {
    stop("This function needs `BayesFactor` to be installed. Please install by running `install.packages('BayesFactor')`.")
  }

  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(var_x, var_y)]
  var_y <- var_y[complete.cases(var_x, var_y)]

  if (x == y) {
    # Avoid error in the case of perfect correlation
    rez <- BayesFactor::correlationBF(rnorm(1000), rnorm(1000), rscale = prior)
    params <- parameters::model_parameters(rez, ...)
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
    rez <- BayesFactor::correlationBF(var_x, var_y, rscale = prior)
    params <- parameters::model_parameters(rez, ...)
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
.cor_test_tetrachoric <- function(data, x, y, ...) {
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

  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    Method = method,
    stringsAsFactors = FALSE
  )
}



#' @importFrom stats complete.cases
#' @importFrom utils capture.output
#' @keywords internal
.cor_test_polychoric <- function(data, x, y, ...) {
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




  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    Method = method,
    stringsAsFactors = FALSE
  )
}




#' @keywords internal
.cor_test_biweight <- function(data, x, y, ...) {
  if (!requireNamespace("WGCNA", quietly = TRUE)) {
    stop("Package `WGCNA` required for tetrachoric correlations. Please install it by running `install.packages('WGCNA').", call. = FALSE)
  }

  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(var_x, var_y)]
  var_y <- var_y[complete.cases(var_x, var_y)]

  # Based on equation of https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3465711/
  u <- (var_x - median(var_x)) / 9 * mad(var_x, constant = 1)
  v <- (var_y - median(var_y)) / 9 * mad(var_y, constant = 1)

  I_x <- ifelse((1 - abs(u)) > 0, 1, 0)
  I_y <- ifelse((1 - abs(v)) > 0, 1, 0)

  wa_x <- (1-u^2)^2 - (I_x * (1 - abs(u)))
  wa_x <- (1-v^2)^2 - (I_y * (1 - abs(v)))




  # Reconstruct dataframe
  dat <- data.frame(as.numeric(var_x), as.numeric(var_y))
  names(dat) <- c(x, y)

  junk <- capture.output(r <- psych::polychoric(dat)$rho[2, 1])
  method <- "Polychoric"




  data.frame(
    Parameter1 = x,
    Parameter2 = y,
    rho = r,
    Method = method,
    stringsAsFactors = FALSE
  )
}

