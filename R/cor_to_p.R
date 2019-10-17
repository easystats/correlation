#' Convert correlation coefficients to p-values
#'
#' Get statistics, p-values and confidence intervals (CI) from correlation coefficients.
#'
#' @param cor A correlation matrix or coefficient.
#' @param n The sample size (number of observations).
#' @inheritParams cor_test
#'
#' @examples
#' cor.test(iris$Sepal.Length, iris$Sepal.Width)
#' cor_to_p(-0.1175698, n = 150, ci = 0.95)
#' cor_to_p(cor(iris[1:4]), n = 150)
#'
#' cor.test(iris$Sepal.Length, iris$Sepal.Width, method = "spearman")
#' cor_to_p(-0.1667777, n = 150, ci = 0.95, method = "spearman")
#'
#' cor.test(iris$Sepal.Length, iris$Sepal.Width, method = "kendall")
#' cor_to_p(-0.07699679, n = 150, ci = 0.95, method = "kendall")
#' @export
cor_to_p <- function(cor, n, ci = 0.95, method = "pearson"){

  # Statistic
  if(method == "kendall"){
    warning("Estimation for Kendall's correlation is not perfectly correct. Help us to improve it.")
    statistic <- (3 * cor * sqrt(n * (n - 1))) / sqrt(2 * (2 * n + 5))
  } else{
    statistic <- cor * sqrt((n - 3) / (1 - cor^2))
  }

  if(is.matrix(statistic)){
    diag(statistic) <- 0
  }


  # p-value
  if(method == "kendall"){
    p <- 2 * pnorm(-abs(statistic))
  } else{
    p <- 2*pt(-abs(statistic), df = n - 2)
  }

  # CI
  se <- 1 / sqrt(n - 3)  # Sample standard error
  ci_low <- tanh(atanh(cor) - 1 * qnorm(1 - (1-ci)/2) * se)
  ci_high <- tanh(atanh(cor) + 1 * qnorm(1 - (1-ci)/2) * se)


  list(p = p, statistic = statistic, ci_low = ci_low, ci_high = ci_high)
}

