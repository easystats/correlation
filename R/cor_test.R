#' Tidy version of cor.test
#'
#' @inheritParams correlation
#' @param x Name of a variable.
#' @param y Name of a variable.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' cor_test(iris, "Petal.Length", "Petal.Width")
#' @export
cor_test <- function(data, x, y, ci = "default", method = "pearson", bayesian = FALSE, iterations = 10^4, rope_full = TRUE, rope_bounds = c(-0.05, 0.05), ...) {
  if (bayesian == FALSE) {
    if (ci == "default") ci <- 0.95
    out <- .cor_test_freq(data, x, y, ci = ci, method = method, ...)
  } else {
    if (ci == "default") ci <- 0.9
    out <- .cor_test_bayes(data, x, y, ci = ci, iterations = iterations, rope_full = rope_full, rope_bounds = rope_bounds)
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






#' @importFrom stats complete.cases mad median
#' @importFrom utils install.packages
#' @keywords internal
.cor_test_bayes <- function(data, x, y, ci = 0.90, iterations = 10^4, rope_full = TRUE, rope_range = c(-0.05, 0.05), prior="medium",  ...) {
  if (!requireNamespace("BayesFactor")) {
    stop("This function needs `BayesFactor` to be installed. Please install by running `install.packages('BayesFactor')`.")
  }

  var_x <- data[[x]]
  var_y <- data[[y]]
  var_x <- var_x[complete.cases(var_x, var_y)]
  var_y <- var_y[complete.cases(var_x, var_y)]

  if(x == y){
    params <- data.frame(
      "Parameter1" = x,
      "Parameter2" = y,
      "Median" = 1,
      "MAD" = 0,
      "CI_low" = 1,
      "CI_high" = 1,
      "pd" = 100,
      "ROPE_Percentage" = 0,
      "BF" = Inf,
      "Prior" = prior
    )
  } else{
    rez <- BayesFactor::correlationBF(var_x, var_y, rscale=prior)
    posterior <- as.data.frame(suppressMessages(BayesFactor::posterior(rez, iterations = iterations, progress = FALSE)))
    posterior <- posterior$rho
    hdi <- bayestestR::hdi(posterior, ci = ci)
    if (rope_full == TRUE) {
      rope <- bayestestR::rope(posterior, range = rope_range, ci = 1)
    } else {
      rope <- bayestestR::rope(posterior, range = rope_range, ci = ci)
    }

    params <- data.frame(
      "Parameter1" = x,
      "Parameter2" = y,
      "Median" = median(posterior),
      "MAD" = mad(posterior),
      "CI_low" = hdi$CI_low,
      "CI_high" = hdi$CI_high,
      "pd" = bayestestR::p_direction(posterior),
      "ROPE_Percentage" = rope$ROPE_Percentage,
      "BF" = exp(rez@bayesFactor$bf),
      "Prior" = prior,
      stringsAsFactors = FALSE
    )
  }


  params
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

