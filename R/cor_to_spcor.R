#' @rdname cor_to_pcor
#' @export
cor_to_spcor <- function(cor = NULL, cov = NULL, tol = .Machine$double.eps^(2 / 3)) {


  # Get cor
  cor <- .get_cor(cor, cov)

  # Semi-partial
  if (is.null(cov)) {
    stop("Covariance matrix (or vector of SD of variables) needs to be passed for semi-partial correlations.")
  } else {
    if (!is.matrix(cov)) {
      cov <- cor_to_cov(cor, sd = cov)
    }
    inverted <- .invert_matrix(cov, tol = tol)
    out <- -cov2cor(inverted) / sqrt(diag(cov)) / sqrt(abs(diag(inverted) - t(t(inverted^2) / diag(inverted))))
  }

  diag(out) <- 1
  out
}
