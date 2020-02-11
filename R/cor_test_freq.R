#' @importFrom stats cor.test
#' @importFrom parameters model_parameters
#' @keywords internal
.cor_test_freq <- function(data, x, y, ci = 0.95, method = "pearson", ...) {
  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  rez <- stats::cor.test(var_x, var_y, conf.level = ci, method = match.arg(tolower(method), c("pearson", "kendall", "spearman"), several.ok = FALSE), alternative = "two.sided")

  params <- parameters::model_parameters(rez)
  params$Parameter1 <- x
  params$Parameter2 <- y

  if (x == y) {
    if ("t" %in% names(params)) params$t <- Inf
    if ("z" %in% names(params)) params$z <- Inf
    if ("S" %in% names(params)) params$S <- Inf
  }

  params
}
