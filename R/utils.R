#' Check if matrix ressembles a correlation matrix
#'
#' @param x A matrix.
#' @return `TRUE` of the matrix is a correlation matrix or `FALSE` otherwise.
#' @export
is.cor <- function(x) {
  square <- isSquare(x)
  symetric <- isSymmetric(x)
  ismatrix <- is.matrix(x)
  diag_one <- all(diag(x) == 1)
  maxi <- max(x) == 1
  all(c(square, symetric, ismatrix, diag_one, maxi))
}


#' Check if Square Matrix
#'
#' @param m A matrix.
#'
#' @return `TRUE` of the matrix is square or `FALSE` otherwise.
#' @export
isSquare <- function(m) {
  if (dim(m)[1] != dim(m)[2]) {
    FALSE
  } else {
    TRUE
  }
}
