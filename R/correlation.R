#' Correlation Analysis
#'
#' Performs a correlation analysis.
#'
#' @param data A dataframe.
#' @param data2 An optional dataframe.
#' @param p_adjust Correction method for frequentist correlations. One of "holm" (default), "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none".
#' @inheritParams cor_test
#'
#'
#' @examples
#' library(dplyr)
#' correlation(iris)
#'
#' iris %>%
#'   select(starts_with("Sepal")) %>%
#'   correlation()
#'
#' iris %>%
#'   select(starts_with("Sepal")) %>%
#'   correlation(select(iris, starts_with("Petal")))
#'
#' iris %>%
#'   group_by(Species) %>%
#'   correlation()
#'
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' correlation(data, method = "auto")
#' @export
correlation <- function(data, data2 = NULL, method = "pearson", p_adjust = "holm", ci = "default", bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), partial = FALSE, partial_include_factors = TRUE, partial_random = FALSE, partial_bayesian = FALSE, ...) {


  # data <- .clean_data(data, include_factors = include_factors, random = random)
  # combinations <- .get_combinations(data, redundant = FALSE, include_factors = include_factors, random = random)
  #
  # for(i in 1:nrow(combinations)){
  #   partialized <- partialize_specific(data,
  #                                      combinations[i, "Parameter1"], combinations[i, "Parameter2"],
  #                                      include_factors = include_factors, random = random, bayesian = bayesian)
  # }


  # Sanity checks
  if (partial == TRUE & bayesian == TRUE) {
    warning("Bayesian partial correlations are not supported yet. Running frequentist analysis...")
    bayesian <- FALSE
  }

  # CI level
  if (bayesian == FALSE) {
    if (ci == "default") ci <- 0.95
  } else {
    if (ci == "default") ci <- 0.89
  }

  if (inherits(data, "grouped_df")) {
    if (!requireNamespace("dplyr")) {
      stop("This function needs `dplyr` to be installed. Please install by running `install.packages('dplyr')`.")
    }

    groups <- dplyr::group_vars(data)
    ungrouped_x <- dplyr::ungroup(data)
    xlist <- split(ungrouped_x, ungrouped_x[groups], sep = " - ")

    if (!is.null(data2)) {
      if (inherits(data2, "grouped_df") & dplyr::group_vars(data2) == groups) {
        ungrouped_y <- dplyr::ungroup(data2)
        ylist <- split(ungrouped_y, ungrouped_y[groups], sep = " - ")
        out <- data.frame()
        for (i in names(xlist)) {
          rez <- .correlation(xlist[[i]][sapply(xlist[[i]], is.numeric)],
            data2 = ylist[[i]][sapply(ylist[[i]], is.numeric)],
            ci = ci,
            method = method,
            bayesian = bayesian,
            p_adjust = p_adjust,
            partial = partial,
            prior = prior
          )
          rez$Group <- i
          out <- rbind(out, rez)
        }
      } else {
        stop("data2 should present the same grouping characteristics than data.")
      }
    } else {
      out <- data.frame()
      for (i in names(xlist)) {
        rez <- .correlation(xlist[[i]],
          data2 = data2,
          ci = ci,
          method = method,
          bayesian = bayesian,
          p_adjust = p_adjust,
          partial = partial,
          prior = prior
        )
        rez$Group <- i
        out <- rbind(out, rez)
      }
    }
    out <- out[c("Group", names(out)[names(out) != "Group"])]
  } else {
    out <- .correlation(data,
      data2 = data2,
      ci = ci,
      method = method,
      bayesian = bayesian,
      p_adjust = p_adjust,
      partial = partial,
      prior = prior
    )
  }

  # Reorder
  out <- out[order(out$Parameter1, out$Parameter2), ]

  attributes(out) <- c(
    attributes(out),
    list(
      "ci" = ci,
      "method" = method,
      "bayesian" = bayesian,
      "p_adjust" = p_adjust,
      "partial" = partial,
      "prior" = prior
    )
  )

  class(out) <- unique(c("easycorrelation", "parameters_model", class(out)))
  out
}



#' @keywords internal
.correlation <- function(data, data2 = NULL, method = "pearson", p_adjust = "holm", ci = "default", bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), partial = FALSE, partial_include_factors = TRUE, partial_random = FALSE, partial_bayesian = FALSE, ...) {

  if(!is.null(data2)){
    data <- cbind(data, data2)
  }


  combinations <- .get_combinations(data, data2 = NULL, redundant = FALSE, include_factors = partial_include_factors, random = partial_random)

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
                       ...)

    # Merge
    if (i == 1) {
      params <- result
    } else {
      if (!all(names(result) %in% names(params))) {
        if ("rho" %in% names(result) & !"rho" %in% names(params)) {
          names(result)[names(result) == "rho"] <- "r"
        }
        result[names(params)[!names(params) %in% names(result)]] <- NA
      }
      params <- rbind(params, result)
    }
  }
  }

  # P-values adjustments
  if ("p" %in% names(params)) {
    params$p <- p.adjust(params$p,
                         method = p_adjust,
                         n = nrow(.get_combinations(data, data2 = data2, redundant = FALSE)))
  }

  if (!is.null(data2)) {
    params <- params[params$Parameter1 %in% vars, ]
    params <- params[params$Parameter2 %in% vars2, ]
  }



  params
}

