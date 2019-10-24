#' Matrix Inversion
#'
#' Performs a Moore-Penrose generalized inverse (also called the Pseudoinverse).
#'
#' @inheritParams cor_to_pcor
#' @examples
#' m <- cor(iris[1:4])
#' matrix_inverse(m)
#' @param m Matrix for which the inverse is required.
#'
#' @seealso pinv from the pracma package
#' @export
matrix_inverse <- function(m, tol = .Machine$double.eps^(2 / 3)) {

  # Sanity checks
  # sanity checks
  if (!isSquare(m)) {
    stop("The matrix should be a square matrix.")
  }

  stopifnot(is.numeric(m), length(dim(m)) == 2, is.matrix(m))

  s <- svd(m)

  p <- (s$d > max(tol * s$d[1], 0))
  if (all(p)) {
    mp <- s$v %*% (1 / s$d * t(s$u))
  } else if (any(p)) {
    mp <- s$v[, p, drop = FALSE] %*% (1 / s$d[p] * t(s$u[, p, drop = FALSE]))
  } else {
    mp <- matrix(0, nrow = ncol(m), ncol = nrow(m))
  }

  colnames(mp) <- colnames(m)
  row.names(mp) <- row.names(m)
  mp
}










#' @keywords internal
.invert_matrix <- function(m, tol = .Machine$double.eps^(2 / 3)) {
  if (det(m) < tol) {
    # The inverse of variance-covariance matrix is calculated using Moore-Penrose generalized matrix invers due to its determinant of zero.
    out <- matrix_inverse(m, tol)
    colnames(out) <- colnames(m)
    row.names(out) <- row.names(m)
  } else {
    out <- solve(m)
  }
  out
}
