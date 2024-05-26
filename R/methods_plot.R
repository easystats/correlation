#' @export
plot.easycorrelation <- function(x, ...) {
  insight::check_if_installed("see", "to plot correlation graphs")

  plot(visualisation_recipe(x, ...), ...)
}

#' @export
plot.easycormatrix <- plot.easycorrelation

#' @export
plot.easycor_test <- plot.easycorrelation
