#' Correlation test
#'
#' This function performs a correlation test between two variables.
#' You can easily visualize the result using [`plot()`][visualisation_recipe.easycormatrix()] (see examples [**here**](https://easystats.github.io/correlation/reference/visualisation_recipe.easycormatrix.html#ref-examples)).
#'
#' @param data A data frame.
#' @param x,y Names of two variables present in the data.
#' @param ci Confidence/Credible Interval level. If `"default"`, then it is
#'   set to `0.95` (`95%` CI).
#' @param method A character string indicating which correlation coefficient is
#'   to be used for the test. One of `"pearson"` (default), `"kendall"`,
#'   `"spearman"` (but see also the `ranktransform` argument), `"biserial"`,
#'   `"polychoric"`, `"tetrachoric"`, `"biweight"`, `"distance"`, `"percentage"`
#'   (for percentage bend correlation), `"blomqvist"` (for Blomqvist's
#'   coefficient), `"hoeffding"` (for Hoeffding's D), `"gamma"`, `"gaussian"`
#'   (for Gaussian Rank correlation) or `"shepherd"` (for Shepherd's Pi
#'   correlation). Setting `"auto"` will attempt at selecting the most relevant
#'   method (polychoric when ordinal factors involved, tetrachoric when
#'   dichotomous factors involved, point-biserial if one dichotomous and one
#'   continuous and pearson otherwise). See below the **details** section for a
#'   description of these indices.
#' @param bayesian If `TRUE`, will run the correlations under a Bayesian
#'   framework.
#' @param partial_bayesian If partial correlations under a Bayesian framework
#'   are needed, you will also need to set `partial_bayesian` to `TRUE` to
#'   obtain "full" Bayesian partial correlations. Otherwise, you will obtain
#'   pseudo-Bayesian partial correlations (i.e., Bayesian correlation based on
#'   frequentist partialization).
#' @param include_factors If `TRUE`, the factors are kept and eventually
#'   converted to numeric or used as random effects (depending of `multilevel`).
#'   If `FALSE`, factors are removed upfront.
#' @param partial Can be `TRUE` or `"semi"` for partial and semi-partial
#'   correlations, respectively.
#' @inheritParams datawizard::adjust
#' @param bayesian_prior For the prior argument, several named values are
#'   recognized: `"medium.narrow"`, `"medium"`, `"wide"`, and `"ultrawide"`.
#'   These correspond to scale values of `1/sqrt(27)`, `1/3`, `1/sqrt(3)` and
#'   `1`, respectively. See the `BayesFactor::correlationBF` function.
#' @param bayesian_ci_method,bayesian_test See arguments in
#'   [`parameters::model_parameters()`] for `BayesFactor` tests.
#' @param ranktransform If `TRUE`, will rank-transform the variables prior to
#'   estimating the correlation, which is one way of making the analysis more
#'   resistant to extreme values (outliers). Note that, for instance, a
#'   Pearson's correlation on rank-transformed data is equivalent to a
#'   Spearman's rank correlation. Thus, using `ranktransform=TRUE` and
#'   `method="spearman"` is redundant. Nonetheless, it is an easy option to
#'   increase the robustness of the correlation as well as flexible way to
#'   obtain Bayesian or multilevel Spearman-like rank correlations.
#' @param winsorize Another way of making the correlation more "robust" (i.e.,
#'   limiting the impact of extreme values). Can be either `FALSE` or a number
#'   between 0 and 1 (e.g., `0.2`) that corresponds to the desired threshold.
#'   See the [`datawizard::winsorize()`] function for more details.
#' @param bootstrap If `TRUE`, the confidence interval, standard error, and
#'   p-value are obtained from a nonparametric percentile bootstrap instead of
#'   the analytic formulas: rows are resampled with replacement `iterations`
#'   times and the coefficient is recomputed on each resample. See the
#'   'Bootstrap' section. Not available for Bayesian, partial, or multilevel
#'   correlations.
#' @param iterations Number of bootstrap resamples (default `1000`). Only used
#'   when `bootstrap = TRUE` or `cluster` is given.
#' @param cluster Name of a column in `data` identifying clusters of rows
#'   (for example, participants with repeated measures). If given, a cluster
#'   bootstrap is run (`bootstrap` is set to `TRUE`): whole clusters, rather
#'   than rows, are resampled with replacement. See the 'Bootstrap' section.
#' @param verbose Toggle warnings.
#' @param ... Additional arguments (e.g., `alternative`) to be passed to
#'   other methods. See `stats::cor.test` for further details.
#'
#'
#' @inherit correlation details
#' @inherit correlation references
#'
#' @return A data frame of class `easycor_test` with the coefficient, its
#'   confidence interval, the test statistic, and the p-value. Under bootstrap
#'   (`bootstrap = TRUE` or `cluster`), the object also has an `SE` column and
#'   three attributes: `ci_method` (`"bootstrap"` or `"cluster-bootstrap"`),
#'   `iterations` (the number of replicates kept after dropping the failed
#'   ones), and `bootstrap_replicates` (the kept replicate coefficients, from
#'   which `CI_low`, `CI_high`, `SE`, and, except for distance correlation and
#'   Hoeffding's D, `p` are computed). With fewer than three complete
#'   observations the bootstrap is skipped and neither the `SE` column nor
#'   these attributes are added.
#'
#' @examples
#' library(correlation)
#'
#' cor_test(iris, "Sepal.Length", "Sepal.Width")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "spearman")
#' \donttest{
#' # Bootstrap confidence interval, standard error, and p-value
#' cor_test(iris, "Sepal.Length", "Sepal.Width", bootstrap = TRUE, iterations = 200)
#'
#' # Cluster bootstrap: rows of the same cluster are resampled together
#' data <- iris
#' data$id <- rep(1:30, 5)
#' cor_test(data, "Sepal.Length", "Sepal.Width", cluster = "id", iterations = 200)
#'
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "kendall")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "biweight")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "distance")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "percentage")
#'
#' if (require("wdm", quietly = TRUE)) {
#'   cor_test(iris, "Sepal.Length", "Sepal.Width", method = "blomqvist")
#' }
#'
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
#' # Robust (these two are equivalent)
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "spearman")
#' cor_test(iris, "Sepal.Length", "Sepal.Width", method = "pearson", ranktransform = TRUE)
#'
#' # Winsorized
#' cor_test(iris, "Sepal.Length", "Sepal.Width", winsorize = 0.2)
#'
#' # Tetrachoric
#' if (require("psych", quietly = TRUE) && require("rstanarm", quietly = TRUE)) {
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
#' # Partial
#' cor_test(iris, "Sepal.Length", "Sepal.Width", partial = TRUE)
#' if (require("lme4", quietly = TRUE)) {
#'   cor_test(iris, "Sepal.Length", "Sepal.Width", multilevel = TRUE)
#' }
#' if (require("rstanarm", quietly = TRUE)) {
#'   cor_test(iris, "Sepal.Length", "Sepal.Width", partial_bayesian = TRUE)
#' }
#' }
#' @export
cor_test <- function(
  data,
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
  ranktransform = FALSE,
  winsorize = FALSE,
  bootstrap = FALSE,
  iterations = 1000,
  cluster = NULL,
  verbose = TRUE,
  ...
) {
  # valid matrix checks
  if (!all(x %in% names(data)) || !all(y %in% names(data))) {
    insight::format_error(
      "The names you entered for x and y are not available in the dataset. Make sure there are no typos!"
    )
  }

  if (ci == "default") {
    ci <- 0.95
  }
  if (!partial && (partial_bayesian || multilevel)) {
    partial <- TRUE
  }

  # Bootstrap arguments
  if (!is.null(cluster)) {
    bootstrap <- TRUE
  }
  if (!isTRUE(bootstrap) && !isFALSE(bootstrap)) {
    insight::format_error("`bootstrap` must be `TRUE` or `FALSE`.")
  }
  if (bootstrap) {
    if (bayesian) {
      insight::format_error(
        "Bootstrap confidence intervals are not available for Bayesian correlations (`bayesian = TRUE`)."
      )
    }
    if (!isFALSE(partial) || multilevel) {
      insight::format_error(
        "Bootstrap confidence intervals are not available for partial or multilevel correlations (`partial`, `partial_bayesian`, or `multilevel`)."
      )
    }
    if (
      !is.numeric(iterations) ||
        length(iterations) != 1L ||
        is.na(iterations) ||
        iterations < 2
    ) {
      insight::format_error(
        "`iterations` must be a single number of at least 2."
      )
    }
    iterations <- as.integer(floor(iterations))
    if (!is.null(cluster)) {
      if (
        !is.character(cluster) ||
          length(cluster) != 1L ||
          !cluster %in% names(data)
      ) {
        insight::format_error(
          "`cluster` must be the name of one column in the data."
        )
      }
      if (cluster %in% c(x, y)) {
        insight::format_error(
          "`cluster` must name a column other than `x` and `y`."
        )
      }
      n_clusters <- length(unique(data[[cluster]][
        stats::complete.cases(data[c(x, y, cluster)])
      ]))
      if (n_clusters < 2L) {
        insight::format_error(
          "`cluster` must have at least 2 distinct values among the complete cases."
        )
      }
      if (n_clusters < 20L && isTRUE(verbose)) {
        insight::format_warning(sprintf(
          "`cluster` has only %d distinct values. The accuracy of the cluster bootstrap is driven by the number of clusters, not the number of rows; with fewer than 20 clusters the interval can undercover.",
          n_clusters
        ))
      }
    }
  }

  # Make sure factor is no factor
  if (!method %in% c("tetra", "tetrachoric", "poly", "polychoric")) {
    data[c(x, y)] <- datawizard::to_numeric(
      data[c(x, y)],
      dummy_factors = FALSE
    )
  }

  # However, for poly, we need factors!
  if (
    method %in%
      c("poly", "polychoric") &&
      all(vapply(data[c(x, y)], is.numeric, FUN.VALUE = TRUE))
  ) {
    # convert all input to factors, but only if all input currently is numeric
    # we allow mix of numeric and factors
    data[c(x, y)] <- datawizard::to_factor(data[c(x, y)])
  }

  # Partial
  if (!isFALSE(partial)) {
    # partial
    if (isTRUE(partial)) {
      data[[x]] <- datawizard::adjust(
        data[names(data) != y],
        multilevel = multilevel,
        bayesian = partial_bayesian
      )[[x]]
      data[[y]] <- datawizard::adjust(
        data[names(data) != x],
        multilevel = multilevel,
        bayesian = partial_bayesian
      )[[y]]
    }

    # semi-partial
    if (partial == "semi") {
      insight::format_error(
        "Semi-partial correlations are not supported yet. Get in touch if you want to contribute."
      )
    }
  }

  # Winsorize
  if (!isFALSE(winsorize) && !is.null(winsorize)) {
    # set default (if not specified)
    if (isTRUE(winsorize)) {
      winsorize <- 0.2
    }

    # winsorize the complete cases of x and y in place, keeping the other
    # columns (the cluster column among them) and the incomplete rows
    complete <- stats::complete.cases(data[c(x, y)])
    data[complete, c(x, y)] <- as.data.frame(
      datawizard::winsorize(
        data[complete, c(x, y)],
        threshold = winsorize,
        verbose = verbose
      )
    )
  }

  # Rank transform (i.e., "robust")
  if (ranktransform) {
    data[c(x, y)] <- datawizard::ranktransform(
      data[c(x, y)],
      sign = FALSE,
      method = "average"
    )
  }

  # check if enough no. of obs ------------------------------

  # this is a trick in case the number of valid observations is lower than 3
  n_obs <- length(.complete_variable_x(data, x, y))
  invalid <- FALSE
  if (n_obs < 3L) {
    if (isTRUE(verbose)) {
      insight::format_warning(paste(
        x,
        "and",
        y,
        "have less than 3 complete observations. Returning NA."
      ))
    }
    invalid <- TRUE
    original_info <- list(data = data, x = x, y = y)
    data <- datasets::mtcars # Basically use a working dataset so the correlation doesn't fail
    x <- "mpg"
    y <- "disp"
  }

  # Find method
  method <- tolower(method)
  if (method == "auto" && !bayesian) {
    method <- .find_correlationtype(data, x, y)
  }
  if (method == "auto" && bayesian) {
    method <- "pearson"
  }

  # Frequentist
  if (!bayesian) {
    out <- .cor_test_frequentist(data, x, y, ci = ci, method = method, ...)

    # Bootstrap: replace the analytic CI, SE, and p by resampled ones
    if (bootstrap && !invalid) {
      out <- .cor_test_bootstrap(
        out,
        data,
        x,
        y,
        ci = ci,
        method = method,
        cluster = cluster,
        iterations = iterations,
        verbose = verbose,
        ...
      )
      # column reordering below drops custom attributes; re-attach at the end
      bootstrap_attributes <- attributes(out)[c(
        "ci_method",
        "iterations",
        "bootstrap_replicates"
      )]
    }

    # Bayesian
  } else if (method %in% c("tetra", "tetrachoric")) {
    insight::format_error(
      "Tetrachoric Bayesian correlations are not supported yet. Get in touch if you want to contribute."
    )
  } else if (method %in% c("poly", "polychoric")) {
    insight::format_error(
      "Polychoric Bayesian correlations are not supported yet. Get in touch if you want to contribute."
    )
  } else if (method %in% c("biserial", "pointbiserial", "point-biserial")) {
    insight::format_error(
      "Biserial Bayesian correlations are not supported yet. Get in touch if you want to contribute."
    )
  } else if (method == "biweight") {
    insight::format_error(
      "Biweight Bayesian correlations are not supported yet. Get in touch if you want to contribute."
    )
  } else if (method == "distance") {
    insight::format_error(
      "Bayesian distance correlations are not supported yet. Get in touch if you want to contribute."
    )
  } else if (
    method %in% c("percentage", "percentage_bend", "percentagebend", "pb")
  ) {
    insight::format_error(
      "Bayesian Percentage Bend correlations are not supported yet. Get in touch if you want to contribute."
    )
  } else if (method %in% c("blomqvist", "median", "medial")) {
    insight::format_error(
      "Bayesian Blomqvist correlations are not supported yet. Check-out the BBcor package (https://github.com/donaldRwilliams/BBcor)."
    )
  } else if (method == "hoeffding") {
    insight::format_error(
      "Bayesian Hoeffding's correlations are not supported yet. Check-out the BBcor package (https://github.com/donaldRwilliams/BBcor)."
    )
  } else if (method == "gamma") {
    insight::format_error(
      "Bayesian gamma correlations are not supported yet. Get in touch if you want to contribute."
    )
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

  # Replace by NANs if invalid
  if (isTRUE(invalid)) {
    data <- original_info$data
    out$Parameter1 <- original_info$x
    out$Parameter2 <- original_info$y
    out[!names(out) %in% c("Parameter1", "Parameter2")] <- NA
  }

  # Number of observations and CI
  out$n_Obs <- n_obs
  out$CI <- ci

  # Reorder columns
  if ("CI_low" %in% names(out)) {
    col_order <- c(
      "Parameter1",
      "Parameter2",
      "r",
      "rho",
      "tau",
      "Dxy",
      "SE",
      "CI",
      "CI_low",
      "CI_high"
    )
    out <- out[c(
      col_order[col_order %in% names(out)],
      setdiff(colnames(out), col_order[col_order %in% names(out)])
    )]
  }

  # Output
  attr(out, "coefficient_name") <- c("rho", "r", "tau", "Dxy")[
    c("rho", "r", "tau", "Dxy") %in% names(out)
  ][1]
  attr(out, "ci") <- ci
  attr(out, "data") <- data
  if (bootstrap && !invalid) {
    attributes(out) <- c(attributes(out), bootstrap_attributes)
  }
  class(out) <- unique(c(
    "easycor_test",
    "easycorrelation",
    "parameters_model",
    class(out)
  ))
  out
}


# Utilities ---------------------------------------------------------------

#' @keywords internal
.cor_test_frequentist <- function(
  data,
  x,
  y,
  ci = 0.95,
  method = "pearson",
  ...
) {
  if (method %in% c("tetra", "tetrachoric")) {
    .cor_test_tetrachoric(data, x, y, ci = ci, ...)
  } else if (method %in% c("poly", "polychoric")) {
    .cor_test_polychoric(data, x, y, ci = ci, ...)
  } else if (method %in% c("biserial", "pointbiserial", "point-biserial")) {
    .cor_test_biserial(data, x, y, ci = ci, method = method, ...)
  } else if (method == "biweight") {
    .cor_test_biweight(data, x, y, ci = ci, ...)
  } else if (method == "distance") {
    .cor_test_distance(data, x, y, ci = ci, ...)
  } else if (
    method %in% c("percentage", "percentage_bend", "percentagebend", "pb")
  ) {
    .cor_test_percentage(data, x, y, ci = ci, ...)
  } else if (method %in% c("blomqvist", "median", "medial")) {
    .cor_test_blomqvist(data, x, y, ci = ci, ...)
  } else if (method == "hoeffding") {
    .cor_test_hoeffding(data, x, y, ci = ci, ...)
  } else if (method == "somers") {
    .cor_test_somers(data, x, y, ci = ci, ...)
  } else if (method == "gamma") {
    .cor_test_gamma(data, x, y, ci = ci, ...)
  } else if (method == "gaussian") {
    .cor_test_gaussian(data, x, y, ci = ci, ...)
  } else if (method %in% c("shepherd", "sheperd", "shepherdspi", "pi")) {
    .cor_test_shepherd(data, x, y, ci = ci, bayesian = FALSE, ...)
  } else {
    .cor_test_freq(data, x, y, ci = ci, method = method, ...)
  }
}

#' @keywords internal
.complete_variable_x <- function(data, x, y) {
  data[[x]][stats::complete.cases(data[[x]], data[[y]])]
}

#' @keywords internal
.complete_variable_y <- function(data, x, y) {
  data[[y]][stats::complete.cases(data[[x]], data[[y]])]
}
