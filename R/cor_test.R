#' Correlation test
#'
#' This function perfoms a correlation test between two variables.
#'
#' @param data A dataframe.
#' @param x,y Names of two variables present in the data.
#' @param ci Confidence/Credible Interval level. If "default", then 0.95 for Frequentist and 0.89 for Bayesian (see documentation in the \pkg{bayestestR} package).
#' @param method A character string indicating which correlation coefficient is to be used for the test. One of "pearson" (default), "kendall", or "spearman", "biserial", "polychoric", "tetrachoric", "biweight", "distance" or "percentage" (for percentage bend correlation). Setting "auto" will attempt at selecting the most relevant method (polychoric when ordinal factors involved, tetrachoric when dichotomous factors involved, point-biserial if one dichotomous and one continuous and pearson otherwise).
#' @param bayesian,partial_bayesian If TRUE, will run the correlations under a Bayesian framework Note that for partial correlations, you will also need to set \code{partial_bayesian} to \code{TRUE} to obtain "full" Bayesian partial correlations. Otherwise, you will obtain pseudo-Bayesian partial correlations (i.e., Bayesian correlation based on frequentist partialization).
#' @param include_factors If \code{TRUE}, the factors are kept and eventually converted to numeric or used as random effects (depending of \code{multilevel}). If \code{FALSE}, factors are removed upfront.
#' @param partial Can be TRUE or "semi" for partial and semi-partial correlations, respectively. This only works for Frequentist correlations.
#' @inheritParams partialize
#' @param bayesian_prior For the prior argument, several named values are recognized: "medium.narrow", "medium", "wide", and "ultrawide". These correspond to scale values of 1/sqrt(27), 1/3, 1/sqrt(3) and 1, respectively. See the \code{BayesFactor::correlationBF} function.
#' @param bayesian_ci_method,bayesian_test See arguments in \code{\link[=parameters]{model_parameters}} for \code{BayesFactor} tests.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @inherit correlation details
#'
#'
#'
#' @examples
#' cor_test(iris, "Sepal.Length", "Sepal.Width")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "spearman")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "kendall")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "biweight")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "distance")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "percentage")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", bayesian = TRUE)
#'
#' data <- iris
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
cor_test <- function(data, x, y, method = "pearson", ci = "default", bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), include_factors = FALSE, partial = FALSE, partial_bayesian = FALSE, multilevel = FALSE, ...) {

  # Sanity checks
  if (partial == FALSE & (partial_bayesian | multilevel)) {
    if (partial_bayesian) {
      warning("`partial` must be set to TRUE in order for `multilevel` to be used. Setting it to TRUE.")
      partial <- TRUE
    }
    if (partial_bayesian) {
      warning("`partial` must be set to TRUE in order for `partial_bayesian` to be used. Setting it to TRUE.")
      partial <- TRUE
    }
  }

  # Partial
  if (partial) {
    data[[x]] <- effectsize::adjust(data[names(data) != y], multilevel = multilevel, bayesian = partial_bayesian)[[x]]
    data[[y]] <- effectsize::adjust(data[names(data) != x], multilevel = multilevel, bayesian = partial_bayesian)[[y]]
  }

  # Frequentist
  if (bayesian == FALSE) {
    if (ci == "default") ci <- 0.95

    if (method == "auto") method <- .cor_test_findtype(data, x, y)

    if (tolower(method) %in% c("tetra", "tetrachoric", "biserial")) {
      out <- .cor_test_tetrachoric(data, x, y, ci = ci, ...)
    } else if (tolower(method) %in% c("poly", "polychoric")) {
      out <- .cor_test_polychoric(data, x, y, ci = ci, ...)
    } else if (tolower(method) %in% c("biweight")) {
      out <- .cor_test_biweight(data, x, y, ci = ci, ...)
    } else if (tolower(method) %in% c("distance")) {
      out <- .cor_test_distance(data, x, y, ci = ci, corrected = TRUE, ...)
    } else if (tolower(method) %in% c("percentage", "percentage_bend", "percentagebend", "pb")) {
      out <- .cor_test_percentage(data, x, y, ci = ci, ...)
    } else {
      out <- .cor_test_freq(data, x, y, ci = ci, method = method, ...)
    }

    # Bayesian
  } else {
    if (ci == "default") ci <- 0.89

    if (method %in% c("tetra", "tetrachoric")) {
      stop("Tetrachoric Bayesian correlations are not supported yet.")
    } else if (tolower(method) %in% c("poly", "polychoric")) {
      stop("Polychoric Bayesian correlations are not supported yet.")
    } else if (tolower(method) %in% c("biweight")) {
      stop("Biweight Bayesian correlations are not supported yet.")
    } else if (tolower(method) %in% c("distance")) {
      stop("Bayesian distance correlations are not supported yet.")
    } else {
      out <- .cor_test_bayes(data, x, y, ci = ci, bayesian_prior = bayesian_prior, bayesian_ci_method = bayesian_ci_method, bayesian_test = bayesian_test, ...)
    }
  }

  attr(out, "ci") <- ci
  class(out) <- unique(c("easycorrelation", "parameters_model", class(out)))
  out
}





# Utilities ---------------------------------------------------------------



#' @keywords internal
.complete_variable_x <- function(data, x, y) {
  data[[x]][complete.cases(data[[x]], data[[y]])]
}

#' @keywords internal
.complete_variable_y <- function(data, x, y) {
  data[[y]][complete.cases(data[[x]], data[[y]])]
}



#' @keywords internal
.cor_test_findtype <- function(data, x, y) {
  if (length(unique(data[[x]])) == 2 | length(unique(data[[y]])) == 2) {
    current_method <- "tetrachoric"
  } else if (is.factor(data[x]) | is.factor(data[y])) {
    current_method <- "polychoric"
  } else {
    current_method <- "pearson"
  }
  current_method
}
