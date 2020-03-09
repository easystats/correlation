#' Convert a correlation to covariance
#'
#' @inheritParams cor_to_pcor
#' @param sd,variance A vector that contains the standard deviations, or the variance, of the variables in the correlation matrix.
#'
#' @return A covariance matrix.
#'
#' @examples
#' cor <- cor(iris[1:4])
#' cov(iris[1:4])
#'
#' cor_to_cov(cor, sd = sapply(iris[1:4], sd))
#' cor_to_cov(cor, variance = sapply(iris[1:4], var))
#' @export
cor_to_cov <- function(cor, sd = NULL, variance = NULL, tol = .Machine$double.eps^(2 / 3)) {

  # sanity checks
  if (!isSquare(cor)) {
    stop("The matrix should be a square matrix.")
  }

  if (is.null(sd)) {
    if (is.null(variance)) {
      stop("SD or variance of variables needs to be provided.")
    } else {
      sd <- sqrt(variance)
    }
  }

  n <- nrow(cor)

  if (n != length(sd)) {
    stop("The length of 'sd' or 'variance' should be the same as the number of rows of the matrix.")
  }

  if (length(sd[sd > 0]) != n) {
    stop("The elements in 'sd' or 'variance' should all be non-negative.")
  }

  if (isSymmetric(cor)) {
    is_symmetric <- TRUE
  } else {
    is_symmetric <- FALSE
  }
  p <- dim(cor)[1]
  q <- p * (p - 1) / 2
  if (isTRUE(all.equal(cor[lower.tri(cor)], rep(0, q))) || isTRUE(all.equal(cor[upper.tri(cor)], rep(0, q)))) {
    is_triangular <- TRUE
  } else {
    is_triangular <- FALSE
  }
  if (!is_symmetric & !is_triangular) {
    stop("'cor' should be either a symmetric or a triangular matrix")
  }

  cov <- diag(sd) %*% cor %*% diag(sd)
  colnames(cov) <- rownames(cov) <- colnames(cor)
  cov
}
