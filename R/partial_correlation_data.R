#' Partial Correlation Data
#'
#' This function can be used to (slowly) get the data of which a regular correlation is equivalent to the partial correlation between two variables (adjusted for the rest of the dataset). As it is based on underlying fitting of multiple regressions, it allows more flexibility, such as including factors as random effects and/or fitting the models under a Bayesian framework. The values returned by this function are the residuals of the regression models.
#'
#' @inheritParams cor_test
#' @param random If \code{TRUE}, the factors are included as random factors. If \code{FALSE} (default), factors are binarized (dummified) and partialized out the same as the other numeric variables.
#'
#' @examples
#' pcordata <- partial_correlation_data(x = "Sepal.Length",
#'                                      y = "Sepal.Width",
#'                                      data = iris[1:4])
#' cor(pcordata)
#'
#' # Is equivalent to
#' cor_to_pcor(cor(iris[1:4]))[1:2, 1:2]
#'
#' \donttest{
#' # Which is close the the Bayesian alternative
#' pcordata <- partial_correlation_data(x = "Sepal.Length",
#'                                      y = "Sepal.Width",
#'                                      data = iris[1:4],
#'                                      bayesian = TRUE)
#'  cor(pcordata)
#' }
#'
#'
#'
#' @export
partial_correlation_data <- function(data, x, y, random = FALSE, bayesian = FALSE){


  if(random == FALSE){
    data <- parameters::convert_data_to_numeric(data)
  }

  nums <- names(data[sapply(data, is.numeric)])
  nums_formula <- nums[!nums %in% c(x, y)]
  if(length(nums_formula) == 0){
    nums_formula <- "1"
  } else{
    nums_formula <-  paste(nums_formula, collapse = " + ")
  }

  facs <- names(data[!sapply(data, is.numeric)])
  if(random == FALSE | length(facs) == 0){
    facs_formula <-  ""
  } else{
    facs_formula <-  paste(" + ", paste0("(1|", facs, ")"), collapse = " + ")
  }

  .correlation_data_get(x, y, data, nums_formula, facs_formula, random, bayesian)
}





#' @keywords internal
.correlation_data_get <- function(x, y, data, nums_formula, facs_formula, random = FALSE, bayesian = FALSE){
  formula1 <- paste0(x, " ~ ", nums_formula, facs_formula)
  m1 <- .correlation_data_model(formula1, data, random, bayesian)

  formula2 <- paste0(y, " ~ ", nums_formula, facs_formula)
  m2 <- .correlation_data_model(formula2, data, random, bayesian)

  out <- data.frame(m1, m2)
  names(out) <- c(x, y)
  out
}






#' @importFrom stats lm residuals
#' @keywords internal
.correlation_data_model <- function(formula, data, random = FALSE, bayesian = FALSE){
  if(random == FALSE){
    if(bayesian == FALSE){
      lm(formula, data = data)$residuals
    } else{
      if (!requireNamespace("rstanarm")) {
        stop("This function needs `rstanarm` to be installed. Please install by running `install.packages('rstanarm')`.")
      }
      rstanarm::stan_glm(formula, data = data, refresh = 0)$residuals
    }
  } else{
    if(bayesian == FALSE){
      if (!requireNamespace("lme4")) {
        stop("This function needs `lme4` to be installed. Please install by running `install.packages('lme4')`.")
      }
      residuals(lme4::lmer(formula, data = data))
    } else{
      if (!requireNamespace("rstanarm")) {
        stop("This function needs `rstanarm` to be installed. Please install by running `install.packages('rstanarm')`.")
      }
      rstanarm::stan_lmer(formula, data = data, refresh = 0)$residuals
    }
  }
}
