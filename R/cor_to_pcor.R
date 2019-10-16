#' Correlation Matrix to (Semi) Partial Correlations
#'
#' Convert a correlation matrix to a (semi)partial correlation matrix. Partial correlations are a measure of the correlation between two variables that remains after controlling for (i.e., "partialling" out) all the other relationships. They can be used for graphical Gaussian models, as they represent the direct interactions between two variables, conditioned on all remaining variables. This means that the squared partial correlation between a predictor X1 and a response variable Y can be interpreted as the proportion of (unique) variance accounted for by X1 relative to the residual or unexplained variance of Y that cannot be accounted by the other variables.
#'
#' The semi-partial correlation is similar to the partial correlation statistic. However, it represents (when squared) the proportion of (unique) variance accounted for by the predictor X1, relative to the total variance of Y. Thus, it might be seen as a better indicator of the "practical relevance" of a predictor, because it is scaled to (i.e., relative to) the total variability in the response variable.
#'
#'
#'
#' @param cor,pcor A (partial) correlation matrix.
#' @param cov An optional covariance matrix (or a vector of the SD of the variables). Required for semi-partial correlations.
#' @param semi Semi-partial correlations.
#' @param tol Relative tolerance to detect zero singular values.
#'
#' @examples
#' library(ppcor)
#'
#' cor <- cor(iris[1:4])
#' pcor <- cor_to_pcor(cor)
#'
#' # Comparison
#' round(pcor - ppcor::pcor(iris[1:4])$estimate, 2)
#'
#' # Semi-partial
#' spcor <- cor_to_pcor(cor, cov = sapply(iris[1:4], sd), semi = TRUE)
#' round(spcor - ppcor::spcor(iris[1:4])$estimate, 2)
#'
#' # Inverse
#' round(pcor_to_cor(pcor) - cor, 2)
#' # round(pcor_to_cor(spcor, semi = TRUE) - cor, 2)
#' @export
cor_to_pcor = function(cor = NULL, cov = NULL, semi = FALSE, tol = .Machine$double.eps^(2 / 3)){


  # Get cor
  cor <- .get_cor(cor, cov)


  # Partial
  if(semi){
    if(is.null(cov)){
      stop("Covariance matrix (or vector of SD of variables) needs to be passed for semi-partial correlations.")
    } else{
      if(!is.matrix(cov)){
        cov <- cor2cov(cor, sd = cov)
      }
      inverted <- .invert_matrix(cov, tol = tol)
      out <- -cov2cor(inverted) / sqrt(diag(cov)) / sqrt(abs(diag(inverted)-t(t(inverted^2) / diag(inverted))))
    }
  } else{
    inverted <- .invert_matrix(cor, tol = tol)
    out <- -cov2cor(inverted)
  }

  diag(out) <- 1
  out
}




#' @rdname cor_to_pcor
#' @export
pcor_to_cor = function(pcor = NULL, cov = NULL, semi = FALSE, tol = .Machine$double.eps^(2 / 3)) {

  # Get cor
  pcor <- .get_cor(pcor, cov)

  # negate off-diagonal entries, then invert
  m <- -pcor
  diag(m) <- -diag(m)


  if(semi){
    stop("Cannot convert semi-partial correlations to correlations yet. We need help for that.")
    # if(is.null(cov)){
    #   stop("Covariance matrix (or vector of SD of variables) needs to be passed for semi-partial correlations.")
    # } else{
    #   if(!is.matrix(cov)){
    #     cov <- cor2cov(pcor, sd = cov)
    #   }
    #   inverted <- inverted * sqrt(diag(cov)) * sqrt(abs(diag(inverted) - t(t(inverted^2) / diag(inverted))))
    # }
  } else{
    inverted <- .invert_matrix(m, tol = tol)
    out <- cov2cor(inverted)
  }

  out
}
























#' @keywords internal
.get_cor <- function(cor = NULL, cov = NULL){
  # Get Cormatrix
  if(is.null(cor)){
    if(is.null(cov)){
      stop("A correlation or covariance matrix is required.")
    } else{
      cor <- cov2cor(cov)
    }
  }
  cor
}



#' @keywords internal
.invert_matrix <- function(m, tol = .Machine$double.eps^(2 / 3)){
  if (det(m) < tol) {
    # The inverse of variance-covariance matrix is calculated using Moore-Penrose generalized matrix invers due to its determinant of zero.
    out <- matrix_inverse(m, tol)
    colnames(out) <- colnames(m)
    row.names(out) <- row.names(m)
  } else{
    out <- solve(m)
  }
  out
}
