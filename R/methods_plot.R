#' @export
plot.easycormatrix <- function(x, ...) {
  insight::check_if_installed("see", "to plot correlation graphs")

  NextMethod()
}


#' @export
plot.easycor_test <- function(x, ...) {
  insight::check_if_installed("see", "to plot correlation graphs")

  plot(visualisation_recipe(x, ...), ...)
}