#' @rdname cormatrix
#' @param cor A correlation matrix. Created by \code{\link{cor}}.
#' @export
cormatrix.matrix <- function(cor, ...){
  if(!is.cor(cor)){
    stop("The input should be a square correlation matrix")
  }

}