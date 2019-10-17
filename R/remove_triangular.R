#' Remove redundant correlations
#'
#' @param cor \link{correlation} object.
#' @param which_column Which column to use for the filtering.
#' @importFrom utils combn
#'
#' @examples
#' cor <- correlation(iris)
#' remove_triangular(cor)
#' @export
remove_triangular <- function(cor, which_column = NULL) {
  rows <- .get_rows_non_NA(summary(cor))
  cor[paste0(cor$Parameter1, "_", cor$Parameter2) %in% rows, ]
}








