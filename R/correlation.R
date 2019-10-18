#' Correlation Analysis
#'
#' Performs a correlation analysis.
#'
#' @param data A dataframe.
#' @param data2 An optional dataframe.
#' @param p_adjust Correction method for frequentist correlations. One of "holm" (default), "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none".
#' @param redundant Shoud the data include redundant rows (where each given correlation is repeated two times).
#' @inheritParams cor_test
#' @inheritParams partialize
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
#'   group_by(Species) %>%
#'   correlation()
#'
#' correlation(mtcars[-2], method = "auto")
#' @export
correlation <- function(data, data2 = NULL, method = "pearson", p_adjust = "holm", ci = "default", bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), redundant = FALSE, include_factors = TRUE, partial = FALSE, partial_random = FALSE, partial_bayesian = FALSE, ...) {

  # CI
  if(ci == "default"){
    if (bayesian){
      ci <- 0.89
    } else{
      ci <- 0.95
    }
  }



  if (inherits(data, "grouped_df")) {
    out <- .correlation_grouped_df(data, data2 = data2, method = method, p_adjust = p_adjust, ci = ci, bayesian = bayesian,bayesian_prior = bayesian_prior,bayesian_ci_method = bayesian_ci_method,bayesian_test = bayesian_test,redundant = redundant,include_factors = include_factors, partial = partial,partial_random = partial_random, partial_bayesian = partial_bayesian)
  } else {
    out <- .correlation(data, data2 = data2, method = method, p_adjust = p_adjust, ci = ci, bayesian = bayesian,bayesian_prior = bayesian_prior,bayesian_ci_method = bayesian_ci_method,bayesian_test = bayesian_test,redundant = redundant,include_factors = include_factors, partial = partial,partial_random = partial_random, partial_bayesian = partial_bayesian)
  }


  attributes(out) <- c(
    attributes(out),
    list(
      "data" = data,
      "data2" = data2,
      "ci" = ci,
      "method" = method,
      "bayesian" = bayesian,
      "p_adjust" = p_adjust,
      "partial" = partial,
      "partial_random" = partial_random,
      "partial_bayesian" = partial_bayesian,
      "bayesian_prior" = bayesian_prior,
      "include_factors" = include_factors
    )
  )

  class(out) <- unique(c("easycorrelation", "parameters_model", class(out)))
  out
}




#' @keywords internal
.correlation_grouped_df <- function(data, data2 = NULL, method = "pearson", p_adjust = "holm", ci = "default", bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), redundant = FALSE, include_factors = TRUE, partial = FALSE, partial_random = FALSE, partial_bayesian = FALSE, ...) {


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

      out <- data.frame()
      for (i in names(xlist)) {
        xlist[[i]][groups] <- NULL
        ylist[[i]][[i]][groups] <- NULL
        rez <- .correlation(xlist[[i]], data2 = ylist[[i]], method = method, p_adjust = p_adjust, ci = ci, bayesian = bayesian,bayesian_prior = bayesian_prior,bayesian_ci_method = bayesian_ci_method,bayesian_test = bayesian_test,redundant = redundant,include_factors = include_factors, partial = partial,partial_random = partial_random, partial_bayesian = partial_bayesian)
        rez$Group <- i
        out <- rbind(out, rez)
      }
    } else {
      stop("data2 should present the same grouping characteristics than data.")
    }
  # else
  } else{
    out <- data.frame()
    for (i in names(xlist)) {
      xlist[[i]][groups] <- NULL
      rez <- .correlation(xlist[[i]], data2, method = method, p_adjust = p_adjust, ci = ci, bayesian = bayesian,bayesian_prior = bayesian_prior,bayesian_ci_method = bayesian_ci_method,bayesian_test = bayesian_test,redundant = redundant,include_factors = include_factors, partial = partial,partial_random = partial_random, partial_bayesian = partial_bayesian)
      rez$Group <- i
      out <- rbind(out, rez)
    }
  }

  # Group as first column
  out <- out[c("Group", names(out)[names(out) != "Group"])]
  out
}







#' @keywords internal
.correlation <- function(data, data2 = NULL, method = "pearson", p_adjust = "holm", ci = "default", bayesian = FALSE, bayesian_prior = "medium", bayesian_ci_method = "hdi", bayesian_test = c("pd", "rope", "bf"), redundant = FALSE, include_factors = TRUE, partial = FALSE, partial_random = FALSE, partial_bayesian = FALSE, ...) {

  if(!is.null(data2)){
    data <- cbind(data, data2)
  }

  # Clean data and get combinations
  combinations <- .get_combinations(data, data2 = NULL, redundant = FALSE, include_factors = include_factors, random = partial_random)
  data <- .clean_data(data, include_factors = include_factors, random = FALSE)


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
        } else if ("r" %in% names(result) & !"r" %in% names(params)) {
          names(params)[names(params) == "rho"] <- "r"
        }
        result[names(params)[!names(params) %in% names(result)]] <- NA
      }
      params <- rbind(params, result)
    }
  }


  # P-values adjustments
  if ("p" %in% names(params)) {
    params$p <- p.adjust(params$p,
                         method = p_adjust,
                         n = nrow(.get_combinations(data, data2 = data2, redundant = FALSE)))
  }

  # Redundant
  if(redundant){
    params <- .add_redundant(params)
  }

  if (!is.null(data2)) {
    params <- params[!params$Parameter1 %in% names(data2), ]
    params <- params[params$Parameter2 %in% names(data2), ]
  }


  params
}


#' @keywords internal
.add_redundant <- function(params){
  inversed <- params
  inversed[, c("Parameter1", "Parameter2")] <- params[, c("Parameter2", "Parameter1")]
  params <- rbind(params, inversed)
  params <- rbind(params, .create_diagonal(params))
  params <- params[order(match(params$Parameter1, names(data)), match(params$Parameter2, names(data))), ]
  params
}




#' @keywords internal
.create_diagonal <- function(params){

  diagonal <- data.frame("Parameter1" = unique(params$Parameter1),
                         "Parameter2" = unique(params$Parameter1))

  if ("Group" %in% names(params)) diagonal$Group <- unique(params$Group)[1]
  if ("r" %in% names(params)) diagonal$r <- 1
  if ("rho" %in% names(params)) diagonal$rho <- 1
  if ("p" %in% names(params)) diagonal$p <- 0
  if ("t" %in% names(params)) diagonal$t <- Inf
  if ("df" %in% names(params)) diagonal$df <- unique(params$df)[1]
  if ("CI_low" %in% names(params)) diagonal$CI_low <- 1
  if ("CI_high" %in% names(params)) diagonal$CI_high <- 1
  if ("Method" %in% names(params)) diagonal$Method <- unique(params$Method)[1]

  if ("Median" %in% names(params)) diagonal$Median <- 1
  if ("Mean" %in% names(params)) diagonal$Mean <- 1
  if ("MAP" %in% names(params)) diagonal$MAP <- 1
  if ("SD" %in% names(params)) diagonal$SD <- 0
  if ("MAD" %in% names(params)) diagonal$MAD <- 0
  if ("CI_low" %in% names(params)) diagonal$CI_low <- 1
  if ("CI_high" %in% names(params)) diagonal$CI_high <- 1
  if ("pd" %in% names(params)) diagonal$pd <- 1
  if ("ROPE_Percentage" %in% names(params)) diagonal$ROPE_Percentage <- 0
  if ("BF" %in% names(params)) diagonal$BF <- Inf
  diagonal
}

