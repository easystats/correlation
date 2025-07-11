#' Correlation Analysis
#'
#' Performs a correlation analysis.
#' You can easily visualize the result using [`plot()`][visualisation_recipe.easycormatrix()]
#' (see examples [**here**](https://easystats.github.io/correlation/reference/visualisation_recipe.easycormatrix.html#ref-examples)).
#'
#' @param data A data frame.
#' @param data2 An optional data frame. If specified, all pair-wise correlations
#'   between the variables in `data` and `data2` will be computed.
#' @param select,select2 (Ignored if `data2` is specified.) Optional names
#'   of variables that should be selected for correlation. Instead of providing
#'   the data frames with those variables that should be correlated, `data`
#'   can be a data frame and `select` and `select2` are (quoted) names
#'   of variables (columns) in `data`. `correlation()` will then
#'   compute the correlation between `data[select]` and
#'   `data[select2]`. If only `select` is specified, all pairwise
#'   correlations between the `select` variables will be computed. This is
#'   a "pipe-friendly" alternative way of using `correlation()` (see
#'   'Examples').
#' @param rename In case you wish to change the names of the variables in
#'   the output, these arguments can be used to specify these alternative names.
#'   Note that the number of names should be equal to the number of columns
#'   selected. Ignored if `data2` is specified.
#' @param p_adjust Correction method for frequentist correlations. Can be one of
#'   `"holm"` (default), `"hochberg"`, `"hommel"`,
#'   `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`,
#'   `"somers"` or `"none"`. See
#'   [stats::p.adjust()] for further details.
#' @param redundant Should the data include redundant rows (where each given
#'   correlation is repeated two times).
#' @param verbose Toggle warnings.
#' @param standardize_names This option can be set to `TRUE` to run
#'   [insight::standardize_names()] on the output to get standardized column
#'   names. This option can also be set globally by running
#'   `options(easystats.standardize_names = TRUE)`.
#' @param missing How should missing values be treated? If `"keep_pairwise"`
#'   (default) then the correlation between each pair of variables is computed
#'   using all complete pairs of observations on those variables. If
#'   `"keep_complete"` then missing values are handled by case-wise deletion,
#'   and correlations are computed using only observations with full data (based
#'   on `data2`/`select`/`select2` when applicable).
#' @inheritParams cor_test
#'
#' @details
#'
#' \subsection{Correlation Types}{
#' - **Pearson's correlation**: This is the most common correlation
#' method. It corresponds to the covariance of the two variables normalized
#' (i.e., divided) by the product of their standard deviations.
#'
#' - **Spearman's rank correlation**: A non-parametric measure of rank
#' correlation (statistical dependence between the rankings of two variables).
#' The Spearman correlation between two variables is equal to the Pearson
#' correlation between the rank values of those two variables; while Pearson's
#' correlation assesses linear relationships, Spearman's correlation assesses
#' monotonic relationships (whether linear or not). Confidence Intervals (CI)
#' for Spearman's correlations are computed using the Fieller et al. (1957)
#' correction (see Bishara and Hittner, 2017).
#'
#' - **Kendall's rank correlation**: In the normal case, the Kendall correlation
#' is preferred than the Spearman correlation because of a smaller gross error
#' sensitivity (GES) and a smaller asymptotic variance (AV), making it more
#' robust and more efficient. However, the interpretation of Kendall's tau is
#' less direct than that of Spearman's rho, in the sense that it quantifies the
#' difference between the percentage of concordant and discordant pairs among
#' all possible pairwise events. Confidence Intervals (CI) for Kendall's
#' correlations are computed using the Fieller et al. (1957) correction (see
#' Bishara and Hittner, 2017).
#'
#' - **Biweight midcorrelation**: A measure of similarity that is
#' median-based, instead of the traditional mean-based, thus being less
#' sensitive to outliers. It can be used as a robust alternative to other
#' similarity metrics, such as Pearson correlation (Langfelder & Horvath,
#' 2012).
#'
#' - **Distance correlation**: Distance correlation measures both
#' linear and non-linear association between two random variables or random
#' vectors. This is in contrast to Pearson's correlation, which can only detect
#' linear association between two random variables.
#'
#' - **Percentage bend correlation**: Introduced by Wilcox (1994), it
#' is based on a down-weight of a specified percentage of marginal observations
#' deviating from the median (by default, `20%`).
#'
#' - **Shepherd's Pi correlation**: Equivalent to a Spearman's rank
#' correlation after outliers removal (by means of bootstrapped Mahalanobis
#' distance).
#'
#' - **Blomqvist’s coefficient**: The Blomqvist’s coefficient (also
#' referred to as Blomqvist's Beta or medial correlation; Blomqvist, 1950) is a
#' median-based non-parametric correlation that has some advantages over
#' measures such as Spearman's or Kendall's estimates (see Shmid & Schimdt,
#' 2006).
#'
#' - **Hoeffding’s D**: The Hoeffding’s D statistics is a
#' non-parametric rank based measure of association that detects more general
#' departures from independence (Hoeffding 1948), including non-linear
#' associations. Hoeffding’s D varies between -0.5 and 1 (if there are no tied
#' ranks, otherwise it can have lower values), with larger values indicating a
#' stronger relationship between the variables.
#'
#' - **Somers’ D**: The Somers’ D statistics is a non-parametric rank
#' based measure of association between a binary variable and a continuous
#' variable, for instance, in the context of logistic regression the binary
#' outcome and the predicted probabilities for each outcome. Usually, Somers' D
#' is a measure of ordinal association, however, this implementation it is
#' limited to the case of a binary outcome.
#'
#' - **Point-Biserial and biserial correlation**: Correlation
#' coefficient used when one variable is continuous and the other is dichotomous
#' (binary). Point-Biserial is equivalent to a Pearson's correlation, while
#' Biserial should be used when the binary variable is assumed to have an
#' underlying continuity. For example, anxiety level can be measured on a
#' continuous scale, but can be classified dichotomously as high/low.
#'
#' - **Gamma correlation**: The Goodman-Kruskal gamma statistic is
#' similar to Kendall's Tau coefficient. It is relatively robust to outliers and
#' deals well with data that have many ties.
#'
#' - **Winsorized correlation**: Correlation of variables that have
#' been formerly Winsorized, i.e., transformed by limiting extreme values to
#' reduce the effect of possibly spurious outliers.
#'
#' - **Gaussian rank Correlation**: The Gaussian rank correlation
#' estimator is a simple and well-performing alternative for robust rank
#' correlations (Boudt et al., 2012). It is based on the Gaussian quantiles of
#' the ranks.
#'
#' - **Polychoric correlation**: Correlation between two theorized
#' normally distributed continuous latent variables, from two observed ordinal
#' variables.
#'
#' - **Tetrachoric correlation**: Special case of the polychoric
#' correlation applicable when both observed variables are dichotomous.
#' }
#'
#' \subsection{Partial Correlation}{
#' **Partial correlations** are estimated as the correlation between two
#' variables after adjusting for the (linear) effect of one or more other
#' variable. The correlation test is then run after having partialized the
#' dataset, independently from it. In other words, it considers partialization
#' as an independent step generating a different dataset, rather than belonging
#' to the same model. This is why some discrepancies are to be expected for the
#' t- and p-values, CIs, BFs etc (but *not* the correlation coefficient)
#' compared to other implementations (e.g., `ppcor`). (The size of these
#' discrepancies depends on the number of covariates partialled-out and the
#' strength of the linear association between all variables.) Such partial
#' correlations can be represented as Gaussian Graphical Models (GGM), an
#' increasingly popular tool in psychology. A GGM traditionally include a set of
#' variables depicted as circles ("nodes"), and a set of lines that visualize
#' relationships between them, which thickness represents the strength of
#' association (see Bhushan et al., 2019).
#'
#' **Multilevel correlations** are a special case of partial correlations where
#' the variable to be adjusted for is a factor and is included as a random
#' effect in a mixed model (note that the remaining continuous variables of the
#' dataset will still be included as fixed effects, similarly to regular partial
#' correlations). The model is a random intercept model, i.e. the multilevel
#' correlation is adjusted for `(1 | groupfactor)`.That said, there is an
#' important difference between using `cor_test()` and `correlation()`: If you
#' set `multilevel=TRUE` in `correlation()` but `partial` is set to `FALSE` (as
#' per default), then a back-transformation from partial to non-partial
#' correlation will be attempted (through [`pcor_to_cor()`][pcor_to_cor]).
#' However, this is not possible when using `cor_test()` so that if you set
#' `multilevel=TRUE` in it, the resulting correlations are partial one. Note
#' that for Bayesian multilevel correlations, if `partial = FALSE`, the back
#' transformation will also recompute *p*-values based on the new *r* scores,
#' and will drop the Bayes factors (as they are not relevant anymore). To keep
#' Bayesian scores, set `partial = TRUE`.
#' }
#'
#' \subsection{Notes}{
#' Kendall and Spearman correlations when `bayesian=TRUE`: These are technically
#' Pearson Bayesian correlations of rank transformed data, rather than pure
#' Bayesian rank correlations (which have different priors).
#' }
#'
#' @return
#'
#' A correlation object that can be displayed using the `print`, `summary` or
#' `table` methods.
#'
#' \subsection{Multiple tests correction}{
#' The `p_adjust` argument can be used to adjust p-values for multiple
#' comparisons. All adjustment methods available in `p.adjust` function
#' `stats` package are supported.
#' }
#'
#' @examplesIf all(insight::check_if_installed(c("psych", "datawizard"), quietly = TRUE)) && getRversion() >= "4.1.0"
#' library(correlation)
#' data(iris)
#'
#' results <- correlation(iris)
#'
#' results
#' summary(results)
#' summary(results, redundant = TRUE)
#'
#' # pipe-friendly usage with  grouped dataframes from {dplyr} package
#' iris |>
#'   correlation(select = "Petal.Width", select2 = "Sepal.Length")
#'
#' # Grouped dataframe
#' # grouped correlations
#' iris |>
#'   datawizard::data_group(Species) |>
#'   correlation()
#'
#' # selecting specific variables for correlation
#' data(mtcars)
#' mtcars |>
#'   datawizard::data_group(am) |>
#'   correlation(select = c("cyl", "wt"), select2 = "hp")
#'
#' # supplying custom variable names
#' correlation(anscombe, select = c("x1", "x2"), rename = c("var1", "var2"))
#'
#' # automatic selection of correlation method
#' correlation(mtcars[-2], method = "auto")
#'
#' @references
#'
#' - Boudt, K., Cornelissen, J., & Croux, C. (2012). The Gaussian rank
#'   correlation estimator: robustness properties. Statistics and Computing,
#'   22(2), 471-483.
#'
#' - Bhushan, N., Mohnert, F., Sloot, D., Jans, L., Albers, C., & Steg, L.
#'   (2019). Using a Gaussian graphical model to explore relationships between
#'   items and variables in environmental psychology research. Frontiers in
#'   psychology, 10, 1050.
#'
#' - Bishara, A. J., & Hittner, J. B. (2017). Confidence intervals for
#'   correlations when data are not normal. Behavior research methods, 49(1),
#'   294-309.
#'
#' - Fieller, E. C., Hartley, H. O., & Pearson, E. S. (1957). Tests for
#'   rank correlation coefficients. I. Biometrika, 44(3/4), 470-481.
#'
#' - Langfelder, P., & Horvath, S. (2012). Fast R functions for robust
#'   correlations and hierarchical clustering. Journal of statistical software,
#'   46(11).
#'
#' - Blomqvist, N. (1950). On a measure of dependence between two random
#'   variables,Annals of Mathematical Statistics,21, 593–600
#'
#' - Somers, R. H. (1962). A new asymmetric measure of association for
#'   ordinal variables. American Sociological Review. 27 (6).
#'
#' @export
correlation <- function(data,
                        data2 = NULL,
                        select = NULL,
                        select2 = NULL,
                        rename = NULL,
                        method = "pearson",
                        missing = "keep_pairwise",
                        p_adjust = "holm",
                        ci = 0.95,
                        bayesian = FALSE,
                        bayesian_prior = "medium",
                        bayesian_ci_method = "hdi",
                        bayesian_test = c("pd", "rope", "bf"),
                        redundant = FALSE,
                        include_factors = FALSE,
                        partial = FALSE,
                        partial_bayesian = FALSE,
                        multilevel = FALSE,
                        ranktransform = FALSE,
                        winsorize = FALSE,
                        verbose = TRUE,
                        standardize_names = getOption("easystats.standardize_names", FALSE),
                        ...) {
  # valid matrix checks
  if (!partial && multilevel) {
    partial <- TRUE
    convert_back_to_r <- TRUE
  } else {
    convert_back_to_r <- FALSE
  }

  # p-adjustment
  if (bayesian) {
    p_adjust <- "none"
  }

  # CI
  if (ci == "default") {
    ci <- 0.95
  }

  if (is.null(data2) && !is.null(select)) {
    # check for valid names
    all_selected <- c(select, select2)
    not_in_data <- !all_selected %in% colnames(data)
    if (any(not_in_data)) {
      insight::format_error(
        paste0("Following variables are not in the data: ", all_selected[not_in_data], collapse = ", ")
      )
    }

    # for grouped df, add group variables to both data frames
    if (inherits(data, "grouped_df")) {
      grp_df <- attributes(data)$groups
      grp_var <- setdiff(colnames(grp_df), ".rows")
      select <- unique(c(select, grp_var))
      select2 <- if (!is.null(select2)) unique(c(select2, grp_var))
    } else {
      grp_df <- NULL
    }


    data2 <- if (!is.null(select2)) data[select2]
    data <- data[select]

    attr(data, "groups") <- grp_df
    attr(data2, "groups") <- if (!is.null(select2)) grp_df
  }

  # renaming the columns if so desired
  if (!is.null(rename)) {
    if (length(data) != length(rename)) {
      insight::format_warning("Mismatch between number of variables and names.")
    } else {
      colnames(data) <- rename
    }
  }

  missing <- insight::validate_argument(missing, options = c("keep_pairwise", "keep_complete"))
  if (missing == "keep_complete") {
    if (is.null(data2)) {
      oo <- stats::complete.cases(data)
      data <- data[which(oo), ]
    } else {
      oo <- stats::complete.cases(cbind(data, data2))
      data <- data[which(oo), ]
      data2 <- data2[which(oo), ]
    }
  }

  if (inherits(data, "grouped_df")) {
    rez <- .correlation_grouped_df(
      data,
      data2 = data2,
      method = method,
      p_adjust = p_adjust,
      ci = ci,
      bayesian = bayesian,
      bayesian_prior = bayesian_prior,
      bayesian_ci_method = bayesian_ci_method,
      bayesian_test = bayesian_test,
      redundant = redundant,
      include_factors = include_factors,
      partial = partial,
      partial_bayesian = partial_bayesian,
      multilevel = multilevel,
      ranktransform = ranktransform,
      winsorize = winsorize,
      verbose = verbose,
      ...
    )
  } else {
    rez <- .correlation(
      data,
      data2 = data2,
      method = method,
      p_adjust = p_adjust,
      ci = ci,
      bayesian = bayesian,
      bayesian_prior = bayesian_prior,
      bayesian_ci_method = bayesian_ci_method,
      bayesian_test = bayesian_test,
      redundant = redundant,
      include_factors = include_factors,
      partial = partial,
      partial_bayesian = partial_bayesian,
      multilevel = multilevel,
      ranktransform = ranktransform,
      winsorize = winsorize,
      verbose = verbose,
      ...
    )
  }
  out <- rez$params

  attributes(out) <- c(
    attributes(out),
    list(
      data = data,
      data2 = data2,
      modelframe = rez$data,
      ci = ci,
      n = nrow(data),
      method = method,
      bayesian = bayesian,
      p_adjust = p_adjust,
      partial = partial,
      multilevel = multilevel,
      partial_bayesian = partial_bayesian,
      bayesian_prior = bayesian_prior,
      include_factors = include_factors,
      missing = missing
    )
  )

  attr(out, "additional_arguments") <- list(...)

  if (inherits(data, "grouped_df")) {
    class(out) <- unique(c("easycorrelation", "see_easycorrelation", "grouped_easycorrelation", "parameters_model", class(out)))
  } else {
    class(out) <- unique(c("easycorrelation", "see_easycorrelation", "parameters_model", class(out)))
  }

  if (convert_back_to_r) out <- pcor_to_cor(pcor = out) # Revert back to r if needed.

  if (standardize_names) insight::standardize_names(out, ...)
  out
}


#' @keywords internal
.correlation_grouped_df <- function(data,
                                    data2 = NULL,
                                    method = "pearson",
                                    p_adjust = "holm",
                                    ci = "default",
                                    bayesian = FALSE,
                                    bayesian_prior = "medium",
                                    bayesian_ci_method = "hdi",
                                    bayesian_test = c("pd", "rope", "bf"),
                                    redundant = FALSE,
                                    include_factors = TRUE,
                                    partial = FALSE,
                                    partial_bayesian = FALSE,
                                    multilevel = FALSE,
                                    ranktransform = FALSE,
                                    winsorize = FALSE,
                                    verbose = TRUE,
                                    ...) {
  groups <- setdiff(colnames(attributes(data)$groups), ".rows")
  ungrouped_x <- as.data.frame(data)
  xlist <- split(ungrouped_x, ungrouped_x[groups], sep = " - ")

  # If data 2 is not provided
  if (is.null(data2)) {
    modelframe <- data.frame()
    out <- data.frame()
    for (i in names(xlist)) {
      xlist[[i]][groups] <- NULL
      rez <- .correlation(
        xlist[[i]],
        data2,
        method = method,
        p_adjust = p_adjust,
        ci = ci,
        bayesian = bayesian,
        bayesian_prior = bayesian_prior,
        bayesian_ci_method = bayesian_ci_method,
        bayesian_test = bayesian_test,
        redundant = redundant,
        include_factors = include_factors,
        partial = partial,
        partial_bayesian = partial_bayesian,
        multilevel = multilevel,
        ranktransform = ranktransform,
        winsorize = winsorize
      )
      modelframe_current <- rez$data
      rez$params$Group <- modelframe_current$Group <- i
      out <- rbind(out, rez$params)
      modelframe <- rbind(modelframe, modelframe_current)
    }
  } else {
    if (inherits(data2, "grouped_df")) {
      groups2 <- setdiff(colnames(attributes(data2)$groups), ".rows")
      if (!all.equal(groups, groups2)) {
        insight::format_error("'data2' should have the same grouping characteristics as data.")
      }
      ungrouped_y <- as.data.frame(data2)
      ylist <- split(ungrouped_y, ungrouped_y[groups], sep = " - ")
      modelframe <- data.frame()
      out <- data.frame()
      for (i in names(xlist)) {
        xlist[[i]][groups] <- NULL
        ylist[[i]][groups] <- NULL
        rez <- .correlation(
          xlist[[i]],
          data2 = ylist[[i]],
          method = method,
          p_adjust = p_adjust,
          ci = ci,
          bayesian = bayesian,
          bayesian_prior = bayesian_prior,
          bayesian_ci_method = bayesian_ci_method,
          bayesian_test = bayesian_test,
          redundant = redundant,
          include_factors = include_factors,
          partial = partial,
          partial_bayesian = partial_bayesian,
          multilevel = multilevel,
          ranktransform = ranktransform,
          winsorize = winsorize
        )
        modelframe_current <- rez$data
        rez$params$Group <- modelframe_current$Group <- i
        out <- rbind(out, rez$params)
        modelframe <- rbind(modelframe, modelframe_current)
      }
    }
  }

  # Group as first column
  out <- out[c("Group", names(out)[names(out) != "Group"])]
  list(params = out, data = modelframe)
}


#' @keywords internal
.correlation <- function(data,
                         data2 = NULL,
                         method = "pearson",
                         p_adjust = "holm",
                         ci = "default",
                         bayesian = FALSE,
                         bayesian_prior = "medium",
                         bayesian_ci_method = "hdi",
                         bayesian_test = c("pd", "rope", "bf"),
                         redundant = FALSE,
                         include_factors = FALSE,
                         partial = FALSE,
                         partial_bayesian = FALSE,
                         multilevel = FALSE,
                         ranktransform = FALSE,
                         winsorize = FALSE,
                         verbose = TRUE,
                         ...) {
  if (!is.null(data2)) {
    data <- cbind(data, data2)
  }

  if (ncol(data) <= 2L && any(sapply(data, is.factor)) && !include_factors) {
    if (isTRUE(verbose)) {
      insight::format_warning("It seems like there is not enough continuous variables in your data. Maybe you want to include the factors? We're setting `include_factors=TRUE` for you.")
    }
    include_factors <- TRUE
  }

  # valid matrix checks ----------------

  # What if only factors
  if (sum(sapply(if (is.null(data2)) data else cbind(data, data2), is.numeric)) == 0) {
    include_factors <- TRUE
  }

  # definitely need factors for polychoric
  if (method == "polychoric") {
    multilevel <- TRUE
    # convert all input to factors, but only if all input currently is numeric
    # we allow mix of numeric and factors
    if (all(vapply(data, is.numeric, FUN.VALUE = TRUE))) {
      data <- datawizard::to_factor(data)
    }
  }

  # Clean data and get combinations -------------

  combinations <- .get_combinations(
    data,
    data2 = NULL,
    redundant = FALSE,
    include_factors = include_factors,
    multilevel = multilevel,
    method = method
  )
  data <- .clean_data(data, include_factors = include_factors, multilevel = multilevel)

  # LOOP ----------------

  for (i in seq_len(nrow(combinations))) {
    x <- as.character(combinations[i, "Parameter1"])
    y <- as.character(combinations[i, "Parameter2"])

    # avoid repeated warnings
    if (i > 1) {
      verbose <- FALSE
    }

    result <- cor_test(
      data,
      x = x,
      y = y,
      ci = ci,
      method = method,
      bayesian = bayesian,
      bayesian_prior = bayesian_prior,
      bayesian_ci_method = bayesian_ci_method,
      bayesian_test = bayesian_test,
      partial = partial,
      multilevel = multilevel,
      ranktransform = ranktransform,
      winsorize = winsorize,
      verbose = verbose,
      ...
    )

    # Merge
    if (i == 1) {
      params <- result
    } else {
      if (!all(names(result) %in% names(params))) {
        if ("r" %in% names(params) && !"r" %in% names(result)) {
          names(result)[names(result) %in% c("rho", "tau")] <- "r"
        }
        if ("r" %in% names(result) && !"r" %in% names(params)) {
          names(params)[names(params) %in% c("rho", "tau")] <- "r"
        }
        if (!"r" %in% names(params) && any(c("rho", "tau") %in% names(result))) {
          names(params)[names(params) %in% c("rho", "tau")] <- "r"
          names(result)[names(result) %in% c("rho", "tau")] <- "r"
        }
        result[names(params)[!names(params) %in% names(result)]] <- NA
      }
      params <- rbind(params, result)
    }
  }

  # Make method column more informative
  if ("Method" %in% names(params)) {
    params$Method <- paste0(params$Method, " correlation")

    # Did Winsorization happen? If yes, Method column should reflect that
    if (!isFALSE(winsorize) && !is.null(winsorize)) {
      params$Method <- paste0("Winsorized ", params$Method)
    }
  }

  # Remove superfluous correlations when two variable sets provided
  if (!is.null(data2)) {
    params <- params[!params$Parameter1 %in% names(data2), ]
    params <- params[params$Parameter2 %in% names(data2), ]
  }

  # P-values adjustments
  if ("p" %in% names(params)) {
    params$p <- stats::p.adjust(params$p, method = p_adjust)
  }

  # Redundant
  if (redundant) {
    params <- .add_redundant(params, data)
  }

  list(params = params, data = data)
}


# plot ----------------------------

#' @export
plot.easycorrelation <- function(x, ...) {
  insight::check_if_installed("see", "to plot correlation graphs")

  NextMethod()
}
