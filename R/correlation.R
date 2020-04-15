#' Correlation Analysis
#'
#' Performs a correlation analysis.
#'
#' @param data A dataframe.
#' @param data2 An optional dataframe.
#' @param p_adjust Correction method for frequentist correlations. Can be one of "holm" (default), "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none".
#' @param redundant Shoud the data include redundant rows (where each given correlation is repeated two times).
#' @inheritParams cor_test
#'
#' @details
#' \subsection{Correlation Types}{
#' \itemize{
#' \item \strong{Pearson's correlation}: The covariance of the two variables divided by the product of their standard deviations.
#' \item \strong{Spearman's rank correlation}: A nonparametric measure of rank correlation (statistical dependence between the rankings of two variables). The Spearman correlation between two variables is equal to the Pearson correlation between the rank values of those two variables; while Pearson's correlation assesses linear relationships, Spearman's correlation assesses monotonic relationships (whether linear or not).
#' \item \strong{Kendall's rank correlation}: In the normal case, the Kendall correlation is preferred than the Spearman correlation because of a smaller gross error sensitivity (GES) and a smaller asymptotic variance (AV), making it more robust and more efficient. However, the interpretation of Kendall's tau is less direct than that of Spearman's rho, in the sense that it quantifies the difference between the \% of concordant and discordant pairs among all possible pairwise events.
#' \item \strong{Biweight midcorrelation}: A measure of similarity between samples that is median-based, rather than mean-based, thus is less sensitive to outliers, and can be a robust alternative to other similarity metrics, such as Pearson correlation.
#' \item \strong{Distance correlation}: Distance correlation measures both linear and nonlinear association between two random variables or random vectors. This is in contrast to Pearson's correlation, which can only detect linear association between two random variables.
#' \item \strong{Percentage bend correlation}: Introduced by Wilcox (1994), it is based on a down-weight of a specified percentage of marginal observations deviating from the median (by default, 20\%).
#' \item \strong{Shepherd's Pi correlation}: Equivalent to a Spearman's rank correlation after outliers removal (by means of bootstrapped mahalanobis distance).
#' \item \strong{Point-Biserial and biserial correlation}: Correlation coefficient used when one variable is continuous and the other is dichotomous (binary). Point-serial is equivalent to a Pearson's correlation, while Biserial should be used when the binary variable is assumed to have an underlying continuity. For example, anxiety level can be measured on a continuous scale, but can be classified dichotomously as high/low.
#' \item \strong{Polychoric correlation}: Correlation between two theorised normally distributed continuous latent variables, from two observed ordinal variables.
#' \item \strong{Tetrachoric correlation}: Special case of the polychoric correlation applicable when both observed variables are dichotomous.
#'
#' \item \strong{Partial correlation}: Correlation between two variables after adjusting for the (linear) the effect of one or more variable. The correlation test is here run after having partialized the dataset, independently from it. In other words, it considers partialization as an independent step generating a different dataset, rather than belonging to the same model. This is why some discrepancies are to be expected for the t- and the p-values (but not the correlation coefficient) compared to other implementations such as \code{ppcor}.
#'
#' \item \strong{Multilevel correlation}: Multilevel correlations are a special case of partial correlations where the variable to be adjusted for is a factor and is included as a random effect in a mixed model.
#'
#' }}
#' \subsection{Notes}{
#' \itemize{
#'   \item Kendall and Spearman correlations when \code{bayesian=TRUE}: These are technically Pearson Bayesian correlations of rank transformed data, rather than pure Bayesian rank correlations (which have different priors).
#'
#' }}
#'
#' @return A correlation object that can be displayed using the \code{print}, \code{summary} or \code{table} methods.
#'
#' \subsection{Multiple tests correction}{
#' About multiple tests corrections.
#' }
#'
#' @examples
#' library(correlation)
#'
#' cor <- correlation(iris)
#'
#' cor
#' summary(cor)
#' summary(cor, redundant = TRUE)
#'
#' # Grouped dataframe
#' if (require("dplyr")) {
#'   library(dplyr)
#'
#'   iris %>%
#'     group_by(Species) %>%
#'     correlation()
#'
#'   correlation(mtcars[-2], method = "auto")
#' }
#' @importFrom stats p.adjust
#' @export
correlation <- function(data, data2 = NULL, method = "pearson", p_adjust = "holm", ci = 0.95, bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), redundant = FALSE, include_factors = FALSE, partial = FALSE, partial_bayesian = FALSE, multilevel = FALSE, robust = FALSE, ...) {

  # Sanity checks
  if (partial == FALSE & multilevel) {
    partial <- TRUE
    convert_back_to_r <- TRUE
  } else {
    convert_back_to_r <- FALSE
  }


  # CI
  if (ci == "default") {
    ci <- 0.95
  }


  if (inherits(data, "grouped_df")) {
    rez <- .correlation_grouped_df(data, data2 = data2, method = method, p_adjust = p_adjust, ci = ci, bayesian = bayesian, bayesian_prior = bayesian_prior, bayesian_ci_method = bayesian_ci_method, bayesian_test = bayesian_test, redundant = redundant, include_factors = include_factors, partial = partial, partial_bayesian = partial_bayesian, multilevel = multilevel, ...)
  } else {
    rez <- .correlation(data, data2 = data2, method = method, p_adjust = p_adjust, ci = ci, bayesian = bayesian, bayesian_prior = bayesian_prior, bayesian_ci_method = bayesian_ci_method, bayesian_test = bayesian_test, redundant = redundant, include_factors = include_factors, partial = partial, partial_bayesian = partial_bayesian, multilevel = multilevel, ...)
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

  class(out) <- unique(c("easycorrelation", "see_easycorrelation", "parameters_model", class(out)))

  if (convert_back_to_r) out <- pcor_to_cor(pcor = out) # Revert back to r if needed.
  out
}




#' @keywords internal
.correlation_grouped_df <- function(data, data2 = NULL, method = "pearson", p_adjust = "holm", ci = "default", bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), redundant = FALSE, include_factors = TRUE, partial = FALSE, partial_bayesian = FALSE, multilevel = FALSE, ...) {
  if (!requireNamespace("dplyr")) {
    stop("This function needs `dplyr` to be installed. Please install by running `install.packages('dplyr')`.")
  }

  groups <- dplyr::group_vars(data)
  ungrouped_x <- dplyr::ungroup(data)
  xlist <- split(ungrouped_x, ungrouped_x[groups], sep = " - ")

  # If data 2 is provided
  if (!is.null(data2)) {
    if (inherits(data2, "grouped_df") & dplyr::group_vars(data2) == groups) {
      ungrouped_y <- dplyr::ungroup(data2)
      ylist <- split(ungrouped_y, ungrouped_y[groups], sep = " - ")

      modelframe <- data.frame()
      out <- data.frame()
      for (i in names(xlist)) {
        xlist[[i]][groups] <- NULL
        ylist[[i]][groups] <- NULL
        rez <- .correlation(xlist[[i]], data2 = ylist[[i]], method = method, p_adjust = p_adjust, ci = ci, bayesian = bayesian, bayesian_prior = bayesian_prior, bayesian_ci_method = bayesian_ci_method, bayesian_test = bayesian_test, redundant = redundant, include_factors = include_factors, partial = partial, partial_bayesian = partial_bayesian, multilevel = multilevel)
        modelframe_current <- rez$data
        rez$params$Group <- modelframe_current$Group <- i
        out <- rbind(out, rez$params)
        modelframe <- rbind(modelframe, modelframe_current)
      }
    } else {
      stop("data2 should present the same grouping characteristics than data.")
    }
    # else
  } else {
    modelframe <- data.frame()
    out <- data.frame()
    for (i in names(xlist)) {
      xlist[[i]][groups] <- NULL
      rez <- .correlation(xlist[[i]], data2, method = method, p_adjust = p_adjust, ci = ci, bayesian = bayesian, bayesian_prior = bayesian_prior, bayesian_ci_method = bayesian_ci_method, bayesian_test = bayesian_test, redundant = redundant, include_factors = include_factors, partial = partial, partial_bayesian = partial_bayesian, multilevel = multilevel)
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
.correlation <- function(data, data2 = NULL, method = "pearson", p_adjust = "holm", ci = "default", bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), redundant = FALSE, include_factors = FALSE, partial = FALSE, partial_bayesian = FALSE, multilevel = FALSE, ...) {
  if (!is.null(data2)) {
    data <- cbind(data, data2)
  }


  # Sanity checks ----------------
  # What if only factors
  if (sum(sapply(if (is.null(data2)) data else cbind(data, data2), is.numeric)) == 0) {
    include_factors <- TRUE
  }

  if (method == "polychoric") multilevel <- TRUE

  # Clean data and get combinations -------------
  combinations <- .get_combinations(data, data2 = NULL, redundant = FALSE, include_factors = include_factors, multilevel = multilevel, method = method)
  data <- .clean_data(data, include_factors = include_factors, multilevel = multilevel)

  # LOOP ----------------
  for (i in 1:nrow(combinations)) {
    x <- as.character(combinations[i, "Parameter1"])
    y <- as.character(combinations[i, "Parameter2"])

    result <- cor_test(data,
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
      ...
    )

    # Merge
    if (i == 1) {
      params <- result
    } else {
      if (!all(names(result) %in% names(params))) {
        if ("r" %in% names(params) & !"r" %in% names(result)) {
          names(result)[names(result) %in% c("rho", "tau")] <- "r"
        }
        if ("r" %in% names(result) & !"r" %in% names(params)) {
          names(params)[names(params) %in% c("rho", "tau")] <- "r"
        }
        if (!"r" %in% names(params) & any(c("rho", "tau") %in% names(result))) {
          names(params)[names(params) %in% c("rho", "tau")] <- "r"
          names(result)[names(result) %in% c("rho", "tau")] <- "r"
        }
        result[names(params)[!names(params) %in% names(result)]] <- NA
      }
      params <- rbind(params, result)
    }
  }


  # P-values adjustments
  if ("p" %in% names(params)) {
    params$p <- stats::p.adjust(params$p,
      method = p_adjust,
      n = nrow(params)
    )
  }

  # Redundant
  if (redundant) {
    params <- .add_redundant(params, data)
  }

  if (!is.null(data2)) {
    params <- params[!params$Parameter1 %in% names(data2), ]
    params <- params[params$Parameter2 %in% names(data2), ]
  }


  list(params = params, data = data)
}







# plot ----------------------------

#' @export
plot.easycorrelation <- function(x, ...) {
  if (!requireNamespace("see", quietly = TRUE)) {
    stop("Package 'see' needed to plot correlation graphs. Please install it by running `install.packages('see')`.")
  }
  NextMethod()
}
