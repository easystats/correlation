#' Tidy version of cor.test
#'
#' @inheritParams correlation
#' @param x Name of a variable.
#' @param y Name of a variable.
#' @param ... Arguments passed to or from other methods (\code{\link[=parameters]{model_parameters.BFBayesFactor}}).
#'
#' @examples
#' cor_test(iris, "Petal.Length", "Petal.Width")
#' @export
cor_test <- function(data, x, y, ci = "default", method = "pearson", bayesian = FALSE, ...) {
  if (bayesian == FALSE) {
    if (ci == "default") ci <- 0.95
    out <- .cor_test_freq(data, x, y, ci = ci, method = method, ...)
  } else {
    if (ci == "default") ci <- 0.89
    out <- .cor_test_bayes(data, x, y, ci = ci, ...)
  }

  out
}










#' @importFrom stats cor.test complete.cases
#' @keywords internal
.cor_test_freq <- function(data, x, y, ci = 0.95, method = "pearson", ...) {
  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(data[[x]], data[[y]])]
  var_y <- var_y[complete.cases(data[[x]], data[[y]])]

  rez <- cor.test(var_x, var_y, conf.level = ci, method = match.arg(method, c("pearson", "kendall", "spearman"), several.ok = FALSE), alternative = "two.sided")

  params <- parameters::model_parameters(rez)
  params$Parameter1 <- x
  params$Parameter2 <- y

  if(x == y){
    if("t" %in% names(params)) params$t <- Inf
    if("z" %in% names(params)) params$z <- Inf
    if("S" %in% names(params)) params$S <- Inf
  }

  params
}






#' @importFrom stats complete.cases rnorm
#' @importFrom utils install.packages
#' @keywords internal
.cor_test_bayes <- function(data, x, y, ci = 0.89, prior="medium",  ...) {
  if (!requireNamespace("BayesFactor")) {
    stop("This function needs `BayesFactor` to be installed. Please install by running `install.packages('BayesFactor')`.")
  }

  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(var_x, var_y)]
  var_y <- var_y[complete.cases(var_x, var_y)]

  if(x == y){
    # Avoid error in the case of perfect correlation
    rez <- BayesFactor::correlationBF(rnorm(1000), rnorm(1000), rscale=prior)
    params <- parameters::model_parameters(rez, ...)
    if("Median" %in% names(params)) params$Median <- 1
    if("Mean" %in% names(params)) params$Mean <- 1
    if("MAP" %in% names(params)) params$MAP <- 1
    if("SD" %in% names(params)) params$SD <- 0
    if("MAD" %in% names(params)) params$MAD <- 0
    if("CI_low" %in% names(params)) params$CI_low <- 1
    if("CI_high" %in% names(params)) params$CI_high <- 1
    if("pd" %in% names(params)) params$pd <- 1
    if("ROPE_Percentage" %in% names(params)) params$ROPE_Percentage <- 0
    if("BF" %in% names(params)) params$BF <- Inf

  } else{
    rez <- BayesFactor::correlationBF(var_x, var_y, rscale=prior)
    params <- parameters::model_parameters(rez, ...)
  }

  # Rename coef
  if(sum(names(params) %in% c("Median", "Mean", "MAP")) == 1){
    names(params)[names(params) %in% c("Median", "Mean", "MAP")] <- "r"
  }

  params <- params[names(params) != "Parameter"]
  params$Parameter1 <- x
  params$Parameter2 <- y
  params[unique(c("Parameter1", "Parameter2", names(params)))]
}




#' #' @examples
#' #' x <- ifelse(iris$Sepal.Width > median(iris$Sepal.Width), 1, 0)
#' #' y <- ifelse(iris$Petal.Width > median(iris$Petal.Width), 1, 0)
#' #' @keywords internal
#' .cor_test_tetrachoric <- function(x, y, ci = 0.95, ...) {
#'
#'   if (!requireNamespace("psych", quietly = TRUE)) {
#'     stop("Package `psych` required for tetrachoric correlations. Please install it.", call. = FALSE)
#'   }
#'
#'   data <- data.frame(x=x, y=y)
#'   rez <- psych::tetrachoric(data)
#'
#'   rez$rho
#'
#' }

