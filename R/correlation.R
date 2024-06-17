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

correlation <- function(data, select = NULL, select2 = NULL,
                        method = "pearson",
                        ci = 0.95,
                        alternative = "two.sided",
                        p_adjust = "holm",
                        use = "pairwise.complete.obs",
                        bayesian = FALSE,
                        include_factors = FALSE,
                        redundant = TRUE,
                        verbose = TRUE,
                        standardize_names = getOption("easystats.standardize_names", FALSE),
                        ...) {
  # algorithm ----

  # step 1: validate input
  # step 2: if needed handle grouped_df type
  # step 3: clean data in accordance to use value:
  #   use == "pairwise.complete.obs" -> perform na.omit on each combination separately
  #   use == "complete.obs" -> perform na.omit on all of the data that are mentioned in the context of select and select2 if there are select and/or select2
  # step 4: splitted to 3 kinds, no select or select2 (a), select only (b) and select and select2 (c)
  #   no select or select2 (a)
  #     step 4.a.1: find all valid combinations of the columns in the data
  #   select only (b)
  #     step 4.b.1: find all valid combinations of the columns in the data that are mentioned in select
  #   select and select2 (c)
  #     step 4.c.1: find all valid combinations of the columns in the data that are mentioned in select and select2
  #   step 4.2: perform correlations on combinations only

  # validate method
  method <- match.arg(tolower(method), c("pearson", "spearman", "spear", "s"))
  # "kendall", "biserial", "pointbiserial",
  # "point-biserial", "rankbiserial",
  # "rank-biserial", "biweight", "distance",
  # "percentage", "percentage_bend",
  # "percentagebend", "pb", "blomqvist",
  # "median", "medial", "hoeffding", "gamma",
  # "gaussian", "shepherd", "sheperd",
  # "shepherdspi", "pi",  "somers", "poly",
  # "polychoric", "tetra", "tetrachoric"))

  # validate alternative
  alternative <- match.arg(tolower(alternative), c("two.sided", "greater", "less"))

  # adjusting variables if bayesian is TRUE
  if(bayesian) {
    p_adjust <- "none"
  }

  # validate p_adjust
  p_adjust <- match.arg(p_adjust, c("holm", "hochberg", "hommel", "bonferroni",
                                    "BH", "BY", "fdr", "somers", "none"))

  # appliance for include_factors or not
  if (include_factors) data <- datawizard::to_numeric(data) else data <- data[sapply(data, is.numeric)]

  # validate ci
  if (ci == "default") {
    ci <- 0.95
  } else if (ci < 0 || ci > 1) {
    insight::format_error("CI should be between 0 and 1.")
  }

  # checking validity of select and select2
  if (is.null(select)) {
    if (!is.null(select2)) {
      insight::format_warning("`select2` is provided but `select` is not, using `select2` as `select`")
      select <- select2
      select2 <- NULL
    }
    else {
      select <- colnames(data)
    }
  }

  # removing values that appear multiple times
  select <- unique(select)
  if (!is.null(select2)) select2 <- unique(select2)

  if (!is.null(select2)) all_selected <- c(select, select2) else all_selected <- select

  not_in_data <- !all_selected %in% colnames(data)

  if (any(not_in_data)) {
    insight::format_error(paste0("Following variables are not in the data: ", all_selected[not_in_data], collapse = ", "))
  }

  # ignore redundant if select2 is given
  if (!is.null(select2) && redundant) {
    redundant <- FALSE
  }

  # validate select/select2
  select <- datawizard::find_columns(data, select = select)
  if (!is.null(select2)) select2 <- datawizard::find_columns(data, select = select2)

  # TODO: support grouped_df

  # # take from it the grouped_df part when i get to it
  # if (!is.null(select)) {
  #   # for grouped df, add group variables to both data frames
  #   if (inherits(data, "grouped_df")) {
  #     grp_df <- attributes(data)$groups
  #     grp_var <- setdiff(colnames(grp_df), ".rows")
  #     select <- unique(c(select, grp_var))
  #     select2 <- if (!is.null(select2)) unique(c(select2, grp_var))
  #   } else {
  #     grp_df <- NULL
  #   }
  #
  #   data2 <- if (!is.null(select2)) data[select2]
  #   data <- data[select]
  #
  #   attr(data, "groups") <- grp_df
  #   attr(data2, "groups") <- if (!is.null(select2)) grp_df
  # }

  # if include_factors is TRUE
  # convert factors to dummy vars

  # if use is "complete.obs"
  if (use == "complete.obs") {
    data <- na.omit(data)
  }

  if (inherits(data, "grouped_df")) {
    # handle as grouped_df
    # rez <- .correlation_grouped_df(
    #   data,
    #   data2 = data2,
    #   method = method,
    #   p_adjust = p_adjust,
    #   ci = ci,
    #   bayesian = bayesian,
    #   bayesian_prior = bayesian_prior,
    #   bayesian_ci_method = bayesian_ci_method,
    #   bayesian_test = bayesian_test,
    #   redundant = redundant,
    #   include_factors = include_factors,
    #   partial = partial,
    #   partial_bayesian = partial_bayesian,
    #   multilevel = multilevel,
    #   ranktransform = ranktransform,
    #   winsorize = winsorize,
    #   verbose = verbose,
    #   ...
    # )
  } else {
    # handle as data frame
    if (ncol(data) <= 2L && any(sapply(data, is.factor)) && !include_factors) {
      if (isTRUE(verbose)) insight::format_error("It seems like there is not enough continuous variables in your data. \nAdding `include_factors = TRUE` to the function should help!")
      include_factors <- TRUE
    }

    # cleaning the data and getting combinations
    combs <- if (is.null(select2)) expand.grid(select, select) else expand.grid(select2, select)
    names(combs) <- c("temp", "var1")
    combs$var2 <- combs$temp
    combs <- sapply(combs[,2:3], as.character)

    # handling redundant if prompted to
    if (redundant) {
      keptL <- NULL
      removeL <- rep(FALSE, nrow(combs))

      for (i in 1:nrow(combs)) {
        if (combs[i, 1] == combs[i, 2]) {
          removeL[i] <- TRUE
          keptL <- c(keptL, combs[i, 1])
        }
        if (combs[i, 2] %in% keptL) {
          removeL[i] <- TRUE
        }
      }
      combs <- combs[!removeL,]
    }

    # running cor test for each pair
    for (i in 1:nrow(combs)) {
      result <- cor_test(x = combs[i, 1], y = combs[i, 2],
                         data = data,
                         method = method,
                         ci = ci,
                         alternative = alternative,
                         bayesian = bayesian,
                         verbose = FALSE,
                         ...)
      if (i > 1) params <- rbind(params, result) else params <- result
    }

    params$p <- stats::p.adjust(params$p, method = p_adjust)
  }

  out <- params

  attributes(out) <- c(
    attributes(out),
    list(
      "data" = data,
      "select" = select,
      "ci" = ci,
      "p_adjust" = p_adjust,
      "use" = use,
      "method" = method,
      "bayesian" = bayesian,
      "include_factors" = include_factors,
      "redundent" = redundant,
      "n" = nrow(data)
    )
  )

  if (!is.null(select2)) {
    attributes(out)$select2 <- select2
  }

  attr(out, "additional_arguments") <- list(...)

  if (inherits(data, "grouped_df")) {
    class(out) <- unique(c("easycorrelation", "see_easycorrelation", "grouped_easycorrelation", "parameters_model", class(out)))
  } else {
    class(out) <- unique(c("easycorrelation", "see_easycorrelation", "parameters_model", class(out)))
  }

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

# plot ----------------------------

#' @export
plot.easycorrelation <- function(x, ...) {
  insight::check_if_installed("see", "to plot correlation graphs")

  NextMethod()
}



