#' @rdname cor_to_pcor
#' @export
spcor_to_cor <- function(spcor = NULL, cov = NULL, semi = FALSE, tol = .Machine$double.eps^(2 / 3)) {
  # Get cor
  spcor <- .get_cor(spcor, cov)

  # negate off-diagonal entries, then invert
  m <- -spcor
  diag(m) <- -diag(m)

  stop("Cannot convert semi-partial correlations to correlations yet. We need help for that.")
  # if(is.null(cov)){
  #   stop("Covariance matrix (or vector of SD of variables) needs to be passed for semi-partial correlations.")
  # } else{
  #   if(!is.matrix(cov)){
  #     cov <- cor_to_cov(spcor, sd = cov)
  #   }
  #   inverted <- inverted * sqrt(diag(cov)) * sqrt(abs(diag(inverted) - t(t(inverted^2) / diag(inverted))))
  # }

  # out
}
