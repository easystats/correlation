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




#' Lists – Generic and Dotted Pairs
#'
#' @rdname list
#' @param x Correlation.
#' @param ... Arguments passed to or from other methods.
#'
#' @method as.list easycorrelation
#' @export
as.list.easycorrelation <- function(x, ...) {
  attributes <- attributes(x)
  attributes$names <- NULL
  attributes$row.names <- NULL
  attributes$class <- NULL
  return(c(list("data" = x),
           attributes))
}




#' Remove redundant correlations
#'
#' @param cor \link{correlation} object.
#' @importFrom utils combn
#' @export
remove_triangular <- function(cor){

  # First proposal
  levels <- levels(cor$Parameter1)
  uniques <- as.character(unique(cor$Parameter1))
  if(!all(uniques %in% as.character(unique(cor$Parameter2)))){
    stop("Correlation matrix must be squared.")
  }
  combinations <- as.data.frame(t(combn(uniques, m=2)), stringsAsFactors = FALSE)
  out <- cor[paste0(cor$Parameter1, cor$Parameter2) %in% paste0(combinations$V1, combinations$V2),]
  levels(out$Parameter1) <- levels
  levels(out$Parameter2) <- levels


  # Second proposal
  out <- data.frame()
  # for(row in 1:nrow(cor)){
  #   current_row <- cor[row, ]
  #   if(!paste(current_row$Parameter2, current_row$Parameter1) %in% paste(out$Parameter1, out$Parameter2)){
  #     out <- rbind(out, current_row)
  #   }
  # }
  # out <- out[out$Parameter1 != out$Parameter2, ]


  return(out)
}


