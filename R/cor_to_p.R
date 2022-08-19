#' Convert correlation to p-values and CIs
#'
#' Get statistics, *p*-values and confidence intervals (CI) from correlation
#' coefficients.
#'
#' @param cor A correlation matrix or coefficient.
#' @param n The sample size (number of observations).
#' @inheritParams cor_test
#'
#' @return A list containing a *p*-value and the statistic or the CI bounds.
#'
#' @examples
#' cor.test(iris$Sepal.Length, iris$Sepal.Width)
#' cor_to_p(-0.1175698, n = 150)
#' cor_to_p(cor(iris[1:4]), n = 150)
#' cor_to_ci(-0.1175698, n = 150)
#' cor_to_ci(cor(iris[1:4]), n = 150)
#'
#' cor.test(iris$Sepal.Length, iris$Sepal.Width, method = "spearman")
#' cor_to_p(-0.1667777, n = 150, method = "spearman")
#' cor_to_ci(-0.1667777, ci = 0.95, n = 150)
#'
#' cor.test(iris$Sepal.Length, iris$Sepal.Width, method = "kendall")
#' cor_to_p(-0.07699679, n = 150, method = "kendall")
#' @references Bishara, A. J., & Hittner, J. B. (2017). Confidence intervals for
#'   correlations when data are not normal. Behavior research methods, 49(1),
#'   294-309.
#'
#' @export
cor_to_p <- function(cor, n, method = "pearson") {
  # Statistic
  if (method == "kendall") {
    warning("Estimation for Kendall's correlation is not perfectly correct. Help us to improve it.", call. = FALSE)
    statistic <- (3 * cor * sqrt(n * (n - 1))) / sqrt(2 * (2 * n + 5))
  } else {
    statistic <- cor * sqrt((n - 2) / (1 - cor^2))
  }

  # p-value
  if (method == "kendall") {
    p <- 2 * stats::pnorm(-abs(statistic))
  } else {
    p <- 2 * stats::pt(-abs(statistic), df = n - 2)
  }

  list(p = p, statistic = statistic)
}
