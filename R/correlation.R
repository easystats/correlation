#' Correlation Analysis
#'
#'
#' @param data A dataframe.
#' @param data2 An optional dataframe.
#' @param ci Confidence/Credible Interval level. If "default", then 0.95 for Frequentist and 0.9 for Bayesian.
#' @param method A character string indicating which correlation coefficient is to be used for the test. One of "pearson" (default), "kendall", or "spearman", can be abbreviated.
#' @param p_adjust Correction method for frequentist correlations. One of "holm" (default), "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none".
#' @param partial Can be TRUE or "semi" for partial and semi-partial correlations, respectively. This only works for Frequentist correlations.
#' @param bayesian If TRUE, the arguments below apply.
#' @param iterations The number of iterations to sample.
#' @param rope_full If TRUE, use the proportion of the entire posterior distribution for the equivalence test. Otherwise, use the proportion of HDI as indicated by the \code{ci} argument.
#' @param rope_bounds \href{https://easystats.github.io/bayestestR/articles/1_IndicesDescription.html#rope}{ROPE's} lower and higher bounds. Should be a list of two values (e.g., \code{c(-0.05, 0.05)}).
#' @param prior For the prior argument, several named values are recognized: "medium.narrow", "medium", "wide", and "ultrawide". These correspond to r scale values of 1/sqrt(27), 1/3, 1/sqrt(3) and 1, respectively. See the \code{BayesFactor::correlationBF} function.
#' @param ... Arguments passed to or from other methods.
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
#' @importFrom dplyr enquos
#' @export
correlation <- function(data, data2 = NULL, ci = "default", method = "pearson", p_adjust = "holm", partial = FALSE, bayesian = FALSE, iterations = 10^4, rope_full = TRUE, rope_bounds = c(-0.05, 0.05), prior="medium", ...) {

  # Sanity checks
  if (partial == TRUE & bayesian == TRUE) {
    warning("Bayesian partial correlations are not supported yet. Running frequentist analysis...")
    bayesian <- FALSE
  }

  # CI level
  if (bayesian == FALSE) {
    if (ci == "default") ci <- 0.95
  } else {
    if (ci == "default") ci <- 0.9
  }

  if (inherits(data, "grouped_df")) {
    groups <- dplyr::group_vars(data)
    ungrouped_x <- dplyr::ungroup(data)
    xlist <- split(ungrouped_x, ungrouped_x[groups], sep = " - ")

    if (!is.null(data2)) {
      if (inherits(data2, "grouped_df") & dplyr::group_vars(data2) == groups) {
        ungrouped_y <- dplyr::ungroup(data2)
        ylist <- split(ungrouped_y, ungrouped_y[groups], sep = " - ")
        out <- data.frame()
        for (i in names(xlist)) {
          rez <- .correlation(dplyr::select_if(xlist[[i]], is.numeric),
            data2 = dplyr::select_if(ylist[[i]], is.numeric),
            ci = ci,
            method = method,
            bayesian = bayesian,
            p_adjust = p_adjust,
            partial=partial,
            iterations = iterations,
            rope_full = rope_full,
            rope_bounds = rope_bounds,
            prior=prior
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
                            partial=partial,
                            iterations = iterations,
                            rope_full = rope_full,
                            rope_bounds = rope_bounds,
                            prior=prior)
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
                        iterations = iterations,
                        rope_full = rope_full,
                        rope_bounds = rope_bounds,
                        prior=prior)
  }

  attributes(out) <- c(attributes(out),
                       list("ci" = ci,
                           "method" = method,
                           "bayesian" = bayesian,
                           "p_adjust" = p_adjust,
                           "partial" = partial,
                           "iterations" = iterations,
                           "rope_full" = rope_full,
                           "rope_bounds" = rope_bounds,
                           "prior"=prior))

  class(out) <- c("easycorrelation", class(out))
  return(out)
}


#' @keywords internal
.correlation <- function(data, data2 = NULL, ci = 0.95, method = "pearson", bayesian = FALSE, p_adjust = "holm", partial = FALSE, iterations = 10^4, rope_full = TRUE, rope_bounds = c(-0.05, 0.05), prior="medium", ...) {
  vars <- names(data)
  vars <- vars[sapply(data[vars], is.numeric)]

  if (is.null(data2)) {
    # combinations <- as.data.frame(t(combn(vars, m=2)),  stringsAsFactors = FALSE)
    # names(combinations) <- c("Var1", "Var2")
    combinations <- expand.grid(vars, vars)
  } else {
    vars2 <- names(data2)
    vars2 <- vars2[sapply(data2[vars2], is.numeric)]
    combinations <- expand.grid(vars, vars2)
    data <- cbind(data, data2)
  }


  if (partial == FALSE) {
    params <- data.frame()
    for (i in 1:nrow(combinations)) {
      result <- cor_test(data,
        x = as.character(combinations$Var1[i]),
        y = as.character(combinations$Var2[i]),
        ci = ci,
        method = method,
        bayesian = bayesian,
        iterations = iterations,
        rope_full = rope_full,
        rope_bounds = rope_bounds,
        prior=prior
      )
      params <- rbind(params, result)
    }
  } else {
    if (partial == TRUE | partial == "full") {
      params <- .partial_correlation(data, method = method, semi = FALSE)
    } else {
      params <- .partial_correlation(data, method = method, semi = TRUE)
    }
    if (!is.null(data2)) {
      params <- dplyr::filter_(params, "Parameter1 %in% vars")
      params <- dplyr::filter_(params, "Parameter2 %in% vars2")
    }
  }

  # P-values adjustments
  if ("p" %in% names(params)) {
    params$p <- p.adjust(params$p, method = p_adjust)
  }

  return(params)
}
