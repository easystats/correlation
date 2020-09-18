#' Correlation test
#'
#' This function performs a correlation test between two variables.
#'
#' @param data A data frame.
#' @param x,y Names of two variables present in the data.
#' @param ci Confidence/Credible Interval level. If "default", then it is set to 0.95 (95\% CI).
#' @param method A character string indicating which correlation coefficient is to be used for the test. One of "pearson" (default), "kendall", or "spearman", "biserial", "polychoric", "tetrachoric", "biweight", "distance", "percentage" (for percentage bend correlation), "blomqvist" (for Blomqvist's coefficient), "hoeffding" (for Hoeffding's D), "gamma", "gaussian" (for Gaussian Rank correlation) or "shepherd" (for Shepherd's Pi correlation). Setting "auto" will attempt at selecting the most relevant method (polychoric when ordinal factors involved, tetrachoric when dichotomous factors involved, point-biserial if one dichotomous and one continuous and pearson otherwise).
#' @param bayesian,partial_bayesian If TRUE, will run the correlations under a Bayesian framework. Note that for partial correlations, you will also need to set \code{partial_bayesian} to \code{TRUE} to obtain "full" Bayesian partial correlations. Otherwise, you will obtain pseudo-Bayesian partial correlations (i.e., Bayesian correlation based on frequentist partialization).
#' @param include_factors If \code{TRUE}, the factors are kept and eventually converted to numeric or used as random effects (depending of \code{multilevel}). If \code{FALSE}, factors are removed upfront.
#' @param partial Can be TRUE or "semi" for partial and semi-partial correlations, respectively.
#' @inheritParams effectsize::adjust
#' @param bayesian_prior For the prior argument, several named values are recognized: "medium.narrow", "medium", "wide", and "ultrawide". These correspond to scale values of 1/sqrt(27), 1/3, 1/sqrt(3) and 1, respectively. See the \code{BayesFactor::correlationBF} function.
#' @param bayesian_ci_method,bayesian_test See arguments in \code{\link[=parameters]{model_parameters}} for \code{BayesFactor} tests.
#' @param robust If TRUE, will rank-transform the variables prior to estimating the correlation. Note that, for instance, a Pearson's correlation on rank-transformed data is equivalent to a Spearman's rank correlation. Thus, using \code{robust=TRUE} and \code{method="spearman"} is redundant. Nonetheless, it is an easy way to increase the robustness of the correlation (as well as obtaining Bayesian Spearman rank Correlations).
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @inherit correlation details
#'
#'
#'
#' @examples
#' library(correlation)
#'
#' cor_test(iris, "Sepal.Length", "Sepal.Width")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "spearman")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "kendall")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "biweight")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "distance")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "percentage")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "blomqvist")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "hoeffding")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "gamma")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "gaussian")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "shepherd")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", bayesian = TRUE)
#'
#' # Tetrachoric
#' data <- iris
#' data$Sepal.Width_binary <- ifelse(data$Sepal.Width > 3, 1, 0)
#' data$Petal.Width_binary <- ifelse(data$Petal.Width > 1.2, 1, 0)
#' cor_test(data, "Sepal.Width_binary", "Petal.Width_binary", method = "tetrachoric")
#'
#' # Biserial
#' cor_test(data, "Sepal.Width", "Petal.Width_binary", method = "biserial")
#'
#' # Polychoric
#' data$Petal.Width_ordinal <- as.factor(round(data$Petal.Width))
#' data$Sepal.Length_ordinal <- as.factor(round(data$Sepal.Length))
#' cor_test(data, "Petal.Width_ordinal", "Sepal.Length_ordinal", method = "polychoric")
#'
#' # When one variable is continuous, will run 'polyserial' correlation
#' cor_test(data, "Sepal.Width", "Sepal.Length_ordinal", method = "polychoric")
#'
#' # Robust (these two are equivalent)
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "pearson", robust = TRUE)
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "spearman", robust = FALSE)
#'
#' # Partial
#' cor_test(iris, "Sepal.Length", "Sepal.Width", partial = TRUE)
#' cor_test(iris, "Sepal.Length", "Sepal.Width", multilevel = TRUE)
#' \donttest{
#' cor_test(iris, "Sepal.Length", "Sepal.Width", partial_bayesian = TRUE)
#' }
#'
#' @importFrom effectsize adjust ranktransform
#' @importFrom stats complete.cases
#' @export
cor_test <- function(data, x, y, method = "pearson", ci = 0.95, bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), include_factors = FALSE, partial = FALSE, partial_bayesian = FALSE, multilevel = FALSE, robust = FALSE, ...) {

  # Sanity checks
  if (!x %in% names(data) | !y %in% names(data)) stop("The names you entered for x and y are not available in the dataset. Make sure there are no typos!")
  if (ci == "default") ci <- 0.95
  if (partial == FALSE & (partial_bayesian | multilevel)) partial <- TRUE

  # Make sure factor is no factor
  if (!method %in% c("tetra", "tetrachoric", "poly", "polychoric")) {
    data[c(x, y)] <- parameters::data_to_numeric(data[c(x, y)], dummy_factors = FALSE)
  }

  # Partial
  if (partial) {
    data[[x]] <- effectsize::adjust(data[names(data) != y], multilevel = multilevel, bayesian = partial_bayesian)[[x]]
    data[[y]] <- effectsize::adjust(data[names(data) != x], multilevel = multilevel, bayesian = partial_bayesian)[[y]]
  }

  # Robust
  if (robust) {
    data[c(x, y)] <- effectsize::ranktransform(data[c(x, y)], sign = FALSE, method = "average")
  }


  # Find method
  method <- tolower(method)
  if (method == "auto" & bayesian == FALSE) method <- .find_correlationtype(data, x, y)
  if (method == "auto" & bayesian == TRUE) method <- "pearson"

  # Frequentist
  if (bayesian == FALSE) {
    if (method %in% c("tetra", "tetrachoric")) {
      out <- .cor_test_tetrachoric(data, x, y, ci = ci, ...)
    } else if (method %in% c("poly", "polychoric")) {
      out <- .cor_test_polychoric(data, x, y, ci = ci, ...)
    } else if (method %in% c("biserial", "pointbiserial", "point-biserial")) {
      out <- .cor_test_biserial(data, x, y, ci = ci, method = method, ...)
    } else if (method %in% c("biweight")) {
      out <- .cor_test_biweight(data, x, y, ci = ci, ...)
    } else if (method %in% c("distance")) {
      out <- .cor_test_distance(data, x, y, ci = ci, ...)
    } else if (method %in% c("percentage", "percentage_bend", "percentagebend", "pb")) {
      out <- .cor_test_percentage(data, x, y, ci = ci, ...)
    } else if (method %in% c("blomqvist", "median", "medial")) {
      out <- .cor_test_blomqvist(data, x, y, ci = ci, ...)
    } else if (method %in% c("hoeffding")) {
      out <- .cor_test_hoeffding(data, x, y, ci = ci, ...)
    } else if (method %in% c("gamma")) {
      out <- .cor_test_gamma(data, x, y, ci = ci, ...)
    } else if (method %in% c("gaussian")) {
      out <- .cor_test_gaussian(data, x, y, ci = ci, ...)
    } else if (method %in% c("shepherd", "sheperd", "shepherdspi", "pi")) {
      out <- .cor_test_shepherd(data, x, y, ci = ci, bayesian = FALSE, ...)
    } else {
      out <- .cor_test_freq(data, x, y, ci = ci, method = method, ...)
    }

    # Bayesian
  } else {
    if (method %in% c("tetra", "tetrachoric")) {
      stop("Tetrachoric Bayesian correlations are not supported yet. Get in touch if you want to contribute.")
    } else if (method %in% c("poly", "polychoric")) {
      stop("Polychoric Bayesian correlations are not supported yet. Get in touch if you want to contribute.")
    } else if (method %in% c("biserial", "pointbiserial", "point-biserial")) {
      stop("Biserial Bayesian correlations are not supported yet. Get in touch if you want to contribute.")
    } else if (method %in% c("biweight")) {
      stop("Biweight Bayesian correlations are not supported yet. Get in touch if you want to contribute.")
    } else if (method %in% c("distance")) {
      stop("Bayesian distance correlations are not supported yet. Get in touch if you want to contribute.")
    } else if (method %in% c("percentage", "percentage_bend", "percentagebend", "pb")) {
      stop("Bayesian Percentage Bend correlations are not supported yet. Get in touch if you want to contribute.")
    } else if (method %in% c("blomqvist", "median", "medial")) {
      stop("Bayesian Blomqvist correlations are not supported yet. Check-out the BBcor package (https://github.com/donaldRwilliams/BBcor).")
    } else if (method %in% c("hoeffding")) {
      stop("Bayesian Hoeffding's correlations are not supported yet. Check-out the BBcor package (https://github.com/donaldRwilliams/BBcor).")
    } else if (method %in% c("gamma")) {
      stop("Bayesian gamma correlations are not supported yet. Get in touch if you want to contribute.")
    } else if (method %in% c("shepherd", "sheperd", "shepherdspi", "pi")) {
      out <- .cor_test_shepherd(data, x, y, ci = ci, bayesian = TRUE, ...)
    } else {
      out <- .cor_test_bayes(data, x, y, ci = ci, method = method, bayesian_prior = bayesian_prior, bayesian_ci_method = bayesian_ci_method, bayesian_test = bayesian_test, ...)
    }
  }

  # Number of observations
  out$n_Obs <- sum(stats::complete.cases(data[[x]], data[[y]]))

  # Reorder columns
  if ("CI_low" %in% names(out)) {
    order <- c("Parameter1", "Parameter2", "r", "rho", "CI_low", "CI_high")
    out <- out[c(order[order %in% names(out)], setdiff(colnames(out), order[order %in% names(out)]))]
  }

  # Output
  attr(out, "ci") <- ci
  class(out) <- unique(c("easycorrelation", "parameters_model", class(out)))
  out
}





# Utilities ---------------------------------------------------------------



#' @keywords internal
.complete_variable_x <- function(data, x, y) {
  data[[x]][stats::complete.cases(data[[x]], data[[y]])]
}

#' @keywords internal
.complete_variable_y <- function(data, x, y) {
  data[[y]][stats::complete.cases(data[[x]], data[[y]])]
}
