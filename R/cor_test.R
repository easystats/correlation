#' Correlation test
#'
#' This function performs a correlation test between two variables.
#'
#' @param data A data frame.
#' @param x,y Names of two variables present in the data.
#' @param ci Confidence/Credible Interval level. If \code{"default"}, then it is
#'   set to 0.95 (95\% CI).
#' @param method A character string indicating which correlation coefficient is
#'   to be used for the test. One of \code{"pearson"} (default),
#'   \code{"kendall"}, \code{"spearman"}, \code{"biserial"},
#'   \code{"polychoric"}, \code{"tetrachoric"}, \code{"biweight"},
#'   \code{"distance"}, \code{"percentage"} (for percentage bend correlation),
#'   \code{"blomqvist"} (for Blomqvist's coefficient), \code{"hoeffding"} (for
#'   Hoeffding's D), \code{"gamma"}, \code{"gaussian"} (for Gaussian Rank
#'   correlation) or \code{"shepherd"} (for Shepherd's Pi correlation). Setting
#'   \code{"auto"} will attempt at selecting the most relevant method
#'   (polychoric when ordinal factors involved, tetrachoric when dichotomous
#'   factors involved, point-biserial if one dichotomous and one continuous and
#'   pearson otherwise).
#' @param bayesian,partial_bayesian If TRUE, will run the correlations under a
#'   Bayesian framework. Note that for partial correlations, you will also need
#'   to set \code{partial_bayesian} to \code{TRUE} to obtain "full" Bayesian
#'   partial correlations. Otherwise, you will obtain pseudo-Bayesian partial
#'   correlations (i.e., Bayesian correlation based on frequentist
#'   partialization).
#' @param include_factors If \code{TRUE}, the factors are kept and eventually
#'   converted to numeric or used as random effects (depending of
#'   \code{multilevel}). If \code{FALSE}, factors are removed upfront.
#' @param partial Can be \code{TRUE} or \code{"semi"} for partial and
#'   semi-partial correlations, respectively.
#' @inheritParams effectsize::adjust
#' @param bayesian_prior For the prior argument, several named values are
#'   recognized: \code{"medium.narrow"}, \code{"medium"}, \code{"wide"}, and
#'   \code{"ultrawide"}. These correspond to scale values of \code{1/sqrt(27)},
#'   \code{1/3}, \code{1/sqrt(3)} and \code{1}, respectively. See the
#'   \code{BayesFactor::correlationBF} function.
#' @param bayesian_ci_method,bayesian_test See arguments in
#'   \code{\link[=parameters]{model_parameters}} for \code{BayesFactor} tests.
#' @param robust If TRUE, will rank-transform the variables prior to estimating
#'   the correlation. Note that, for instance, a Pearson's correlation on
#'   rank-transformed data is equivalent to a Spearman's rank correlation. Thus,
#'   using \code{robust=TRUE} and \code{method="spearman"} is redundant.
#'   Nonetheless, it is an easy way to increase the robustness of the
#'   correlation (as well as obtaining Bayesian Spearman rank Correlations).
#' @param winsorize Either \code{FALSE} or a number between 0 and 1 (e.g.,
#'   \code{0.2}) that corresponds to the threshold of desired
#'   \code{\link[=winsorize]{winsorization}}.
#' @param ... Additional arguments (e.g., \code{alternative}) to be passed to
#'   other methods. \code{\link[stats::cor.test]{cor.test()}} for further
#'   details.
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
#' if (require("wdm", quietly = TRUE)) {
#'   cor_test(iris, "Sepal.Length", "Sepal.Width", method = "blomqvist")
#' }
#' if (require("Hmisc", quietly = TRUE)) {
#'   cor_test(iris, "Sepal.Length", "Sepal.Width", method = "hoeffding")
#' }
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "gamma")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "gaussian")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "shepherd")
#' if (require("BayesFactor", quietly = TRUE)) {
#'   cor_test(iris, "Sepal.Length", "Sepal.Width", bayesian = TRUE)
#' }
#'
#' # Tetrachoric
#' if (require("psych", quietly = TRUE)) {
#'   data <- iris
#'   data$Sepal.Width_binary <- ifelse(data$Sepal.Width > 3, 1, 0)
#'   data$Petal.Width_binary <- ifelse(data$Petal.Width > 1.2, 1, 0)
#'   cor_test(data, "Sepal.Width_binary", "Petal.Width_binary", method = "tetrachoric")
#'
#'   # Biserial
#'   cor_test(data, "Sepal.Width", "Petal.Width_binary", method = "biserial")
#'
#'   # Polychoric
#'   data$Petal.Width_ordinal <- as.factor(round(data$Petal.Width))
#'   data$Sepal.Length_ordinal <- as.factor(round(data$Sepal.Length))
#'   cor_test(data, "Petal.Width_ordinal", "Sepal.Length_ordinal", method = "polychoric")
#'
#'   # When one variable is continuous, will run 'polyserial' correlation
#'   cor_test(data, "Sepal.Width", "Sepal.Length_ordinal", method = "polychoric")
#' }
#'
#' # Robust (these two are equivalent)
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "pearson", robust = TRUE)
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "spearman", robust = FALSE)
#'
#' # Winsorized
#' cor_test(iris, "Sepal.Length", "Sepal.Width", winsorize = 0.2)
#' \dontrun{
#' # Partial
#' cor_test(iris, "Sepal.Length", "Sepal.Width", partial = TRUE)
#' cor_test(iris, "Sepal.Length", "Sepal.Width", multilevel = TRUE)
#' cor_test(iris, "Sepal.Length", "Sepal.Width", partial_bayesian = TRUE)
#' }
#' @importFrom effectsize adjust ranktransform
#' @importFrom parameters data_to_numeric
#' @importFrom stats complete.cases
#' @export
cor_test <- function(data,
                     x,
                     y,
                     method = "pearson",
                     ci = 0.95,
                     bayesian = FALSE,
                     bayesian_prior = "medium",
                     bayesian_ci_method = "hdi",
                     bayesian_test = c("pd", "rope", "bf"),
                     include_factors = FALSE,
                     partial = FALSE,
                     partial_bayesian = FALSE,
                     multilevel = FALSE,
                     robust = FALSE,
                     winsorize = FALSE,
                     ...) {

  # Sanity checks
  if (!x %in% names(data) | !y %in% names(data)) {
    stop("The names you entered for x and y are not available in the dataset. Make sure there are no typos!")
  }

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

  # Winsorize
  if (!isFALSE(winsorize)) {
    data[c(x, y)] <- winsorize(data[c(x, y)], threshold = winsorize)
  }

  # Robust
  if (robust) {
    data[c(x, y)] <- effectsize::ranktransform(data[c(x, y)], sign = FALSE, method = "average")
  }

  n_obs <- length(.complete_variable_x(data, x, y))
  # This is a trick in case the number of valid observations is lower than 3
  invalid <- FALSE
  if (n_obs < 3) {
    warning(paste(x, "and", y, "have less than 3 complete observations. Returning NA."))
    invalid <- TRUE
    original_info <- list(data = data, x = x, y = y)
    data <- datasets::mtcars # Basically use a working dataset so the correlation doesn't fail
    x <- "mpg"
    y <- "disp"
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
    } else if (method %in% c("somers")) {
      out <- .cor_test_somers(data, x, y, ci = ci, ...)
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
      out <- .cor_test_bayes(
        data,
        x,
        y,
        ci = ci,
        method = method,
        bayesian_prior = bayesian_prior,
        bayesian_ci_method = bayesian_ci_method,
        bayesian_test = bayesian_test,
        ...
      )
    }
  }

  # Replace by NANs if invalid
  if (isTRUE(invalid)) {
    data <- original_info$data
    out$Parameter1 <- original_info$x
    out$Parameter2 <- original_info$y
    out[!names(out) %in% c("Parameter1", "Parameter2")] <- NA
  }

  # Number of observations
  out$n_Obs <- n_obs

  # Reorder columns
  if ("CI_low" %in% names(out)) {
    order <- c("Parameter1", "Parameter2", "r", "rho", "Dxy", "CI_low", "CI_high")
    out <- out[c(order[order %in% names(out)], setdiff(colnames(out), order[order %in% names(out)]))]
  }

  # Output
  attr(out, "coefficient_name") <- c("rho", "r", "tau", "Dxy")[c("rho", "r", "tau", "Dxy") %in% names(out)][1]
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
