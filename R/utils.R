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





#' @export
summary.easycorrelation <- function(object, ...){
  summary(as.data.frame(object))
}


#' Remove redundant correlations
#'
#' @param cor \link{correlation} object.
#' @param which_column Which column to use for the filtering.
#' @importFrom utils combn
#' @export
remove_triangular <- function(cor, which_column = NULL){

  if(is.null(which_column)){
    if("r" %in% names(cor)){
      which_column <- "r"
    } else if("Median" %in% names(cor)){
      which_column <- "Median"
    } else{
      stop("Please specify `which_column`.")
    }
  }

  m <- as.table(cor, which_column = which_column)

  # Filter parameter1
  x <- names(m)[!names(m) %in% c("Group", "Parameter")]
  cor <- cor[cor$Parameter1 %in% x,]
  cor$Parameter1 <- factor(cor$Parameter1, levels=x)

  # Filter parameter2
  y <- m$Parameter
  cor <- cor[cor$Parameter2 %in% y,]
  cor$Parameter2 <- factor(cor$Parameter2, levels=rev(y))

  # Remove NANs
  cor <- cor[as.character(cor$Parameter1) != as.character(cor$Parameter2), ]
  for(i in 1:nrow(cor)){
    current_row <- cor[i, ]
    cor[i, which_column] <- m[m$Parameter == current_row$Parameter2, names(m)==current_row$Parameter1]
  }

  cor <- cor[!is.na(cor[[which_column]]),]

  return(cor)
}


