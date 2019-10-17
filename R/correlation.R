#' Correlation Analysis
#'
#' Performs a correlation analysis.
#'
#' @param data A dataframe.
#' @param data2 An optional dataframe.
#' @param ci Confidence/Credible Interval level. If "default", then 0.95 for Frequentist and 0.89 for Bayesian (see documentation in the \pkg{bayestestR} package).
#' @param method A character string indicating which correlation coefficient is to be used for the test. One of "pearson" (default), "kendall", or "spearman", "polychoric", "tetrachoric". Setting "auto" will attempt at selecting the most relevant method (polychoric when ordinal factors involved, tetrachoric when dichotomous factors involved).
#' @param p_adjust Correction method for frequentist correlations. One of "holm" (default), "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none".
#' @param partial Can be TRUE or "semi" for partial and semi-partial correlations, respectively. This only works for Frequentist correlations.
#' @param bayesian If TRUE, the arguments below apply.
#' @param prior For the prior argument, several named values are recognized: "medium.narrow", "medium", "wide", and "ultrawide". These correspond to scale values of 1/sqrt(27), 1/3, 1/sqrt(3) and 1, respectively. See the \code{BayesFactor::correlationBF} function.
#' @param ... Arguments passed to or from other methods (e.g., to \code{\link[=parameters]{model_parameters.BFBayesFactor}}).
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
correlation <- function(data, data2 = NULL, ci = "default", method = "pearson", p_adjust = "holm", partial = FALSE, bayesian = FALSE, prior = "medium", ...) {


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
.correlation <- function(data, data2 = NULL, ci = 0.95, method = "pearson", bayesian = FALSE, p_adjust = "holm", partial = FALSE, prior = "medium", ...) {
  vars <- names(data)
  vars <- vars[sapply(data[vars], is.numeric)]

  if (is.null(data2)) {
    combinations <- expand.grid(vars, vars)
  } else {
    vars2 <- names(data2)
    vars2 <- vars2[sapply(data2[vars2], is.numeric)]
    combinations <- expand.grid(vars, vars2)
    data <- cbind(data, data2)
  }

  # Regular Correlations
  if (partial == FALSE) {
    params <- data.frame()
    for (i in 1:nrow(combinations)) {
      name_x <- as.character(combinations$Var2[i])
      name_y <- as.character(combinations$Var1[i])

      # Auto-Find type
      if (method == "auto") {
        if (length(unique(data[[name_x]])) == 2 | length(unique(data[[name_y]])) == 2) {
          current_method <- "tetrachoric"
        } else if (is.factor(data[name_x]) | is.factor(data[name_y])) {
          current_method <- "polychoric"
        } else {
          current_method <- "pearson"
        }
      } else {
        current_method <- method
      }

      result <- cor_test(data,
        x = name_x,
        y = name_y,
        ci = ci,
        method = current_method,
        bayesian = bayesian,
        prior = prior
      )

      # Merge
      if (nrow(params) == 0) {
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
    # Partial Correlations
  } else {
    if (partial == TRUE | partial == "full") {
      params <- .partial_correlation(data, method = method, semi = FALSE)
    } else {
      params <- .partial_correlation(data, method = method, semi = TRUE)
    }
    if (!is.null(data2)) {
      params <- params[params$Parameter1 %in% vars, ]
      params <- params[params$Parameter2 %in% vars2, ]
    }
  }

  # P-values adjustments
  if ("p" %in% names(params)) {
    params$p <- p.adjust(params$p, method = p_adjust)
  }

  params
}
