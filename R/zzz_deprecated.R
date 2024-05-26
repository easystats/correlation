#' Deprecated functions
#'
#' @param ... Args.
#'
#' @name correlation-deprecated
#'
#' @export
distance_mahalanobis <- function(...) {
  .Defunct('performance::check_outliers(method = "mahalanobis_robust")')
}
