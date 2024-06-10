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
#' @inheritParams cor_test
#'
#' @details
#'
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
#' @examplesIf requireNamespace("poorman", quietly = TRUE) && requireNamespace("psych", quietly = TRUE)
#'
#' library(correlation)
#' library(poorman)
#'
#' results <- correlation(iris)
#'
#' results
#' summary(results)
#' summary(results, redundant = TRUE)
#'
#' # pipe-friendly usage with  grouped dataframes from {dplyr} package
#' iris %>%
#'   correlation(select = "Petal.Width", select2 = "Sepal.Length")
#'
#' # Grouped dataframe
#' # grouped correlations
#' iris %>%
#'   group_by(Species) %>%
#'   correlation()
#'
#' # selecting specific variables for correlation
#' mtcars %>%
#'   group_by(am) %>%
#'   correlation(
#'     select = c("cyl", "wt"),
#'     select2 = c("hp")
#'   )
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

## Notes =========

# files utils_clean_data.R and utils_get_combinations.R are now redundant
# in LOOP can be converted to double loop one runs on the x values and the other on the y values (each time cleaned from the done x values if there is no select2)

# actual function ----

correlation <- function(data,
                        select = NULL,
                        select2 = NULL,
                        method = "pearson",
                        ci = 0.95,
                        p_adjust = "holm",
                        use = "pairwise.complete.obs",
                        bayesian = FALSE,
                        include_factors = FALSE,
                        redundant = FALSE,
                        verbose = TRUE,
                        standardize_names = getOption("easystats.standardize_names", FALSE),
                        ...) {
  # TODO: support grouped_df

  # validate select/select
  select <- datawizard::find_columns(data, select = select)
  select2 <- datawizard::find_columns(data, select = select2)

  if (!is.null(select)) {
    # check for valid names
    all_selected <- c(select, select2)
    not_in_data <- !all_selected %in% colnames(data)
    if (any(not_in_data)) insight::format_error(paste0("Following variables are not in the data: ", all_selected[not_in_data], collapse = ", "))

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

  # if include_factors is TRUE
  # convert factors to dummy vars

  # done further in the correlation function

  # use == "pairwise.complete.obs"
  # use == "complete.obs" -> na.omit(data)

  # ignore redundant if select2 is given

  # adjusting variables if bayesian is TRUE
  if(bayesian) {
    p_adjust <- "none"
  }

  # CI
  if (ci == "default") {
    ci <- 0.95
  }



  # renaming the columns if so desired
  if (!is.null(rename)) {
    if (length(data) != length(rename)) {
      insight::format_warning("Mismatch between number of variables and names.")
    } else {
      colnames(data) <- rename
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
      ci = ci,
      p_adjust = p_adjust,
      use = use,
      bayesian = bayesian,
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
      "data" = data,
      "data2" = data2,
      "modelframe" = rez$data,
      "ci" = ci,
      "n" = nrow(data),
      "method" = method,
      "bayesian" = bayesian,
      "p_adjust" = p_adjust,
      "partial" = partial,
      "multilevel" = multilevel,
      "partial_bayesian" = partial_bayesian,
      "bayesian_prior" = bayesian_prior,
      "include_factors" = include_factors
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
                                    data2 = NULL, # do i need it tho?, maybe except selects instead?
                                    method = "pearson",
                                    ci = 0.95,
                                    p_adjust = "holm",
                                    use = "pairwise.complete.obs",
                                    bayesian = FALSE,
                                    include_factors = FALSE,
                                    redundant = FALSE,
                                    verbose = TRUE,
                                    standardize_names = getOption("easystats.standardize_names", FALSE),
                                    ...) {
  groups <- setdiff(colnames(attributes(data)$groups), ".rows")
  ungrouped_x <- as.data.frame(data)
  xlist <- split(ungrouped_x, ungrouped_x[groups], sep = " - ")

  # If data 2 is provided
  if (!is.null(data2)) {
    if (inherits(data2, "grouped_df")) {
      groups2 <- setdiff(colnames(attributes(data2)$groups), ".rows")
      if (all.equal(groups, groups2)) {
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
      } else {
        stop("'data2' should have the same grouping characteristics as data.", call. = FALSE)
      }
    }
    # else
  } else {
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
  }

  # Group as first column
  out <- out[c("Group", names(out)[names(out) != "Group"])]
  list(params = out, data = modelframe)
}



#' @keywords internal
.correlation <- function(data,
                         data2 = NULL, # do i need it tho?, maybe except selects instead?
                         method = "pearson",
                         ci = 0.95,
                         p_adjust = "holm",
                         use = "pairwise.complete.obs",
                         bayesian = FALSE,
                         include_factors = FALSE,
                         redundant = FALSE,
                         verbose = TRUE,
                         standardize_names = getOption("easystats.standardize_names", FALSE),
                         ...) {
  if (!is.null(data2)) { # do not need data2 then
    data <- cbind(data, data2)
  }

  if (ncol(data) <= 2L && any(sapply(data, is.factor)) && !include_factors) {
    if (isTRUE(verbose)) {
      insight::format_error("It seems like there is not enough continuous variables in your data. \nAdding `include_factors = TRUE` to the function should help!")
    }
    include_factors <- TRUE
  }

  # adjusting variables if bayesian is TRUE
  if(bayesian) {
    if("bayesian_prior" %in% names(list(...))) {
      bayesian_prior <- match.arg(tolower(list(...)$bayesian_prior),
                                  c("medium", "medium.narrow", "wide", "ultra-wide"))
    }

    if ("bayesian_ci_method" %in% names(list(...))) {
      bayesian_ci_method <- list(...)$bayesian_ci_method
    }

    if ("bayesian_test" %in% names(list(...))) {
      bayesian_test <- list(...)$bayesian_test
    }
  }

  # valid matrix checks ----------------

  # What if only factors
  if (sum(sapply(if (is.null(data2)) data else cbind(data, data2), is.numeric)) == 0) {
    include_factors <- TRUE
  }

  if (method == "polychoric") multilevel <- TRUE

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

  # LOOP ----
  # running cor test for each pair
  for (i in seq_len(nrow(combinations))) {
    x <- as.character(combinations[i, "Parameter1"])
    y <- as.character(combinations[i, "Parameter2"])

    # avoid repeated warnings
    if (i > 1) {
      verbose <- FALSE
    }

    result <- cor_test(x, y,
                       data = data,
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

    # reseting for some reason the "rho" and "tau" names to be "r", might not work anymore due to changes in cor_test, option "Dxy" ignored, not really relevant tho

    # will be redundent because cor_test always returns "r" as the name

    # Merge
    if (i == 1) {
      params <- result # first iteration keeps the result of the first pair
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
  }

  # instead of removing the redundant rows, should not compute them at all

  # Remove superfluous correlations when two variable sets provided
  if (!is.null(data2)) {
    params <- params[!params$Parameter1 %in% names(data2), ]
    params <- params[params$Parameter2 %in% names(data2), ]
  }

  # there is always p, then should always do that without the id check

  # P-values adjustments
  if ("p" %in% names(params)) {
    params$p <- stats::p.adjust(params$p, method = p_adjust)
  }

  # again instead of removing the redundant rows, should not compute them at all

  # Redundant
  if (redundant) {
    params <- .add_redundant(params, data)
  }

  list(params = params, data = data)
}

# internal helping functions ----

# without a change
#' @keywords internal
.clean_data <- function(data, include_factors = TRUE, multilevel = FALSE) {
  if (!multilevel) {
    if (include_factors) {
      data <- datawizard::to_numeric(data)
    } else {
      data <- data[sapply(data, is.numeric)]
    }
  }
  data
}

# to be changed
#' @keywords internal
.get_combinations <- function(data,
                              data2 = NULL,
                              redundant = TRUE,
                              include_factors = TRUE,
                              multilevel = FALSE,
                              method = "pearson") {
  data <- .clean_data(data, include_factors = include_factors, multilevel = multilevel)

  if (method == "polychoric") {
    vars <- names(data)
  } else if (multilevel) {
    vars <- names(data[sapply(data, is.numeric)])
  } else {
    vars <- names(data)
  }



  # Find pairs
  if (is.null(data2)) {
    vars2 <- vars
  } else {
    data2 <- .clean_data(data2, include_factors = include_factors, multilevel = multilevel)
    data2_nums <- data2[sapply(data2, is.numeric)]
    vars2 <- names(data2_nums)
  }

  combinations <- expand.grid(vars, vars2, stringsAsFactors = FALSE)
  combinations <- combinations[order(match(combinations$Var1, vars), match(combinations$Var2, vars2)), ]

  row.names(combinations) <- NULL
  names(combinations) <- c("Parameter1", "Parameter2")

  if (!redundant) {
    combinations <- .remove_redundant(combinations)
  }

  combinations
}



# plot ----------------------------

#' @export
plot.easycorrelation <- function(x, ...) {
  insight::check_if_installed("see", "to plot correlation graphs")

  NextMethod()
}



