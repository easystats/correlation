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
  .check_if_square(m)
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

  return(mp)
}
