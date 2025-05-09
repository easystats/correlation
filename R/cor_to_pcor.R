#' Correlation Matrix to (Semi) Partial Correlations
#'
#' Convert a correlation matrix to a (semi)partial correlation matrix. Partial
#' correlations are a measure of the correlation between two variables that
#' remains after controlling for (i.e., "partialling" out) all the other
#' relationships. They can be used for graphical Gaussian models, as they
#' represent the direct interactions between two variables, conditioned on all
#' remaining variables. This means that the squared partial correlation between
#' a predictor X1 and a response variable Y can be interpreted as the proportion
#' of (unique) variance accounted for by X1 relative to the residual or
#' unexplained variance of Y that cannot be accounted by the other variables.
#'
#' The semi-partial correlation is similar to the partial correlation statistic.
#' However, it represents (when squared) the proportion of (unique) variance
#' accounted for by the predictor X1, relative to the total variance of Y. Thus,
#' it might be seen as a better indicator of the "practical relevance" of a
#' predictor, because it is scaled to (i.e., relative to) the total variability
#' in the response variable.
#'
#' @param cor A correlation matrix, or a partial or a semipartial
#'   correlation matrix.
#' @param pcor A correlation matrix, or a partial or a semipartial
#'   correlation matrix.
#' @param cov A covariance matrix (or a vector of the SD of the variables).
#'   Required for semi-partial correlations.
#' @param tol Relative tolerance to detect zero singular values.
#'
#' @return The (semi) partial correlation matrix.
#'
#' @examples
#' cor <- cor(iris[1:4])
#'
#' # Partialize
#' cor_to_pcor(cor)
#' cor_to_spcor(cor, cov = sapply(iris[1:4], sd))
#'
#' # Inverse
#' round(pcor_to_cor(cor_to_pcor(cor)) - cor, 2) # Should be 0
#' @export

cor_to_pcor <- function(cor, tol = .Machine$double.eps^(2 / 3)) {
  UseMethod("cor_to_pcor")
}


#' @export
cor_to_pcor.matrix <- function(cor, tol = .Machine$double.eps^(2 / 3)) {
  cor <- .get_cor(cor, cov = NULL)
  .cor_to_pcor(cor)
}


#' @export
cor_to_pcor.easycormatrix <- function(cor, tol = .Machine$double.eps^(2 / 3)) {
  if (inherits(cor, "matrix")) {
    NextMethod()
  } else {
    .cor_to_pcor_easycormatrix(cor = cor, tol = tol)
  }
}


#' @export

cor_to_pcor.easycorrelation <- function(cor, tol = .Machine$double.eps^(2 / 3)) {
  .cor_to_pcor_easycorrelation(cor = cor, tol = tol)
}


# pcor to cor -------------------------------------------------------------


#' @rdname cor_to_pcor
#' @export
pcor_to_cor <- function(pcor, tol = .Machine$double.eps^(2 / 3)) {
  UseMethod("pcor_to_cor")
}


#' @export
pcor_to_cor.matrix <- function(pcor, tol = .Machine$double.eps^(2 / 3)) {
  pcor <- .get_cor(pcor, cov = NULL)
  .pcor_to_cor(pcor)
}


#' @export
pcor_to_cor.easycormatrix <- function(pcor, tol = .Machine$double.eps^(2 / 3)) {
  if (inherits(pcor, "matrix")) {
    NextMethod()
  } else {
    .cor_to_pcor_easycormatrix(pcor = pcor, tol = tol)
  }
}


#' @export
pcor_to_cor.easycorrelation <- function(pcor, tol = .Machine$double.eps^(2 / 3)) {
  .cor_to_pcor_easycorrelation(pcor = pcor, tol = tol)
}


# Convenience Functions --------------------------------------------------------

#' @keywords internal
.cor_to_pcor_easycorrelation <- function(pcor = NULL, cor = NULL, tol = .Machine$double.eps^(2 / 3)) {
  if (is.null(cor)) {
    r <- .pcor_to_cor(.get_cor(summary(pcor, redundant = TRUE), cov = NULL))
    cor <- pcor
  } else {
    r <- .cor_to_pcor(.get_cor(summary(cor, redundant = TRUE), cov = NULL))
  }

  # Extract info
  p_adjust <- attributes(cor)$p_adjust
  number_obs <- as.matrix(attributes(summary(cor, redundant = TRUE))$n_Obs[-1])

  # Get Statistics
  p <- cor_to_p(r, n = number_obs, method = "pearson")
  ci_vals <- cor_to_ci(r, n = number_obs, ci = attributes(cor)$ci)

  # Replace
  newdata <- data.frame()
  for (i in seq_len(nrow(cor))) {
    row_index <- row.names(r) == cor[i, "Parameter1"]
    col_index <- colnames(r) == cor[i, "Parameter2"]
    newdata <- rbind(
      newdata,
      data.frame(
        r = r[row_index, col_index],
        CI_low = ci_vals$CI_low[row_index, col_index],
        CI_high = ci_vals$CI_high[row_index, col_index],
        t = p$statistic[row_index, col_index],
        df_error = number_obs[row_index, col_index] - 2,
        p = p$p[row_index, col_index],
        Method = "Pearson",
        n_Obs = number_obs[row_index, col_index],
        stringsAsFactors = FALSE
      )
    )
  }

  # Fix for spearman
  if (any(cor$Method %in% c("Spearman", "Kendall"))) {
    newdata$df <- NULL
    if (any(cor$Method == "Spearman")) {
      names(newdata)[names(newdata) == "t"] <- "S"
      newdata$Method <- "Spearman"
    } else {
      names(newdata)[names(newdata) == "t"] <- "z"
      newdata$Method <- "Kendall"
    }
  }

  # Format
  newdata <- cbind(cor[1:2], newdata)
  cor <- cor[, seq_len(ncol(newdata))]
  cor[, ] <- newdata
  names(cor) <- names(newdata)

  # P-values adjustments
  cor$p <- stats::p.adjust(cor$p, method = p_adjust, n = nrow(cor))
  attributes(cor)$p_adjust <- p_adjust

  cor
}


#' @keywords internal
.cor_to_pcor_easycormatrix <- function(pcor = NULL, cor = NULL, tol = .Machine$double.eps^(2 / 3)) {
  if (is.null(cor)) {
    r <- .pcor_to_cor(.get_cor(pcor, cov = NULL))
    cor <- pcor
  } else {
    r <- .cor_to_pcor(.get_cor(cor, cov = NULL))
  }

  # Extract info
  if (inherits(cor, "matrix")) {
    return(r)
  }

  p_adjust <- attributes(cor)$p_adjust
  number_obs <- as.matrix(attributes(cor)$n_Obs[-1])

  p <- cor_to_p(r, n = number_obs, method = "pearson")
  ci_vals <- cor_to_ci(r, n = number_obs, ci = attributes(cor)$ci)
  r <- cbind(data.frame(Parameter = row.names(r)), r)
  row.names(r) <- NULL

  # P-values adjustments
  n_comp <- sum(upper.tri(p$p))
  p$p[upper.tri(p$p)] <- stats::p.adjust(p$p[upper.tri(p$p)], method = p_adjust, n = n_comp)
  p$p[lower.tri(p$p)] <- stats::p.adjust(p$p[lower.tri(p$p)], method = p_adjust, n = n_comp)
  attributes(cor)$p_adjust <- p_adjust

  # Statistic and p-value
  attributes(cor)$pd <- attributes(cor)$BF <- NULL
  attributes(cor)$p[-1] <- p$p
  attributes(cor)$t[-1] <- p$statistic
  attributes(cor)$CI_low[-1] <- ci_vals$CI_low
  attributes(cor)$CI_high[-1] <- ci_vals$CI_high

  attributes(r) <- attributes(cor)
  r
}


#' @keywords internal
.cor_to_pcor <- function(cor, tol = .Machine$double.eps^(2 / 3)) {
  # Get cor
  cor <- .get_cor(cor, cov = NULL)

  # Partial
  inverted <- .invert_matrix(cor, tol = tol)
  out <- -stats::cov2cor(inverted)

  diag(out) <- 1
  out
}


#' @keywords internal
.pcor_to_cor <- function(pcor, tol = .Machine$double.eps^(2 / 3)) {
  # negate off-diagonal entries, then invert
  m <- -pcor
  diag(m) <- -diag(m)

  inverted <- .invert_matrix(m, tol = tol)
  out <- stats::cov2cor(inverted)

  out
}


# Internals ---------------------------------------------------------------

#' @keywords internal
.get_cor <- function(cor = NULL, cov = NULL) {
  # Get Cormatrix
  if (is.null(cor)) {
    if (is.null(cov)) {
      insight::format_error("A correlation or covariance matrix is required.")
    }
    cor <- stats::cov2cor(cov)
  } else if (inherits(cor, "easycormatrix") && colnames(cor)[1] == "Parameter") {
    row.names(cor) <- cor$Parameter
    cor <- as.matrix(cor[-1])
  }
  cor
}
