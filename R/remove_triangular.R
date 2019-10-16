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









#' @keywords internal
.get_rows_non_NA <- function(m) {
  rows <- c()
  cols <- c()

  for (col in names(m)[-1]) {
    for (row in 1:nrow(m)) {
      if (!is.na(m[row, col])) {
        rows <- c(rows, m$Parameter[row])
        cols <- c(cols, col)
      }
    }
  }

  paste0(rows, "_", cols)
}
