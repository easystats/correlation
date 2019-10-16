#' Correlation Matrix Object
#'
#' @param x A matrix x.
#' @param ... rguments passed to or from other methods.
#'
#' @export
cormatrix <- function(...){
  UseMethod("cormatrix")
}


#' @rdname cormatrix
#' @export
as.cormatrix <- cormatrix



#' @rdname cormatrix
#' @export
is.cormatrix <- function(x){
  square <- isSquare(x)
  symetric <- isSymmetric(x)
  ismatrix <- is.matrix(x)
}




#' Check if Square Matrix
#'
#' @param m A matrix.
#'
#' @export
isSquare <- function(m){
  if(dim(m)[1] != dim(m)[2]){
    FALSE
  } else{
    TRUE
  }
}