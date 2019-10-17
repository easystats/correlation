#' Correlation Matrix Object
#'
#' @param ... rguments passed to or from other methods.
#'
#' @export
cormatrix <- function(...){
  UseMethod("cormatrix")
}





#' @rdname cormatrix
#' @export
as.cormatrix <- cormatrix












#' @param x A matrix x.
#' @rdname cormatrix
#' @export
is.cor <- function(x){
  square <- isSquare(x)
  symetric <- isSymmetric(x)
  ismatrix <- is.matrix(x)
  diag_one <- all(diag(x) == 1)
  maxi <- max(x) == 1
  if(any(c(square, symetric, ismatrix, diag_one, maxi) == FALSE)){
    FALSE
  } else{
    TRUE
  }
}







#' @rdname cormatrix
#' @export
is.cormatrix <- function(x){
  inherits(x, "cormatrix")
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