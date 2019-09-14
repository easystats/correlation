#' Moore-Penrose Generalized Inverse
#' @param A Matrix for which the Moore-Penrose inverse is required.
#' @param tol Relative tolerance to detect zero singular values.
#' @seealso pinv from the pracma package
#' @export
generalized_inverse <- function(A, tol = .Machine$double.eps^(2 / 3)) {
  stopifnot(is.numeric(A), length(dim(A)) == 2, is.matrix(A))

  s <- svd(A)

  p <- (s$d > max(tol * s$d[1], 0))
  if (all(p)) {
    mp <- s$v %*% (1 / s$d * t(s$u))
  } else if (any(p)) {
    mp <- s$v[, p, drop = FALSE] %*% (1 / s$d[p] * t(s$u[, p, drop = FALSE]))
  } else {
    mp <- matrix(0, nrow = ncol(A), ncol = nrow(A))
  }

  return(mp)
}
