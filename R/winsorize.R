#' Winsorize data
#'
#' Winsorizization of data.
#'
#' @param data Dataframe or vector.
#' @param threshold The amount of Winsorization.
#'
#' @examples
#' library(correlation)
#'
#' winsorize(iris$Sepal.Length, threshold=0.2)
#' winsorize(iris, threshold=0.2)
#' @export
winsorize <- function(data, threshold=0.2){
  UseMethod("winsorize")
}


#' @export
winsorize.factor <- function(data, threshold=0.2){
  data
}

#' @export
winsorize.character <- winsorize.factor

#' @export
winsorize.logical <- winsorize.factor

#' @export
winsorize.data.frame <- function(data, threshold=0.2){
  sapply(data, winsorize, threshold=threshold)
}


#' @export
winsorize.numeric <- function(data, threshold=0.2){
  # TODO: This function should go in the future data package.
  y<-sort(data)
  n<-length(data)
  ibot<-floor(threshold*n)+1
  itop<-length(data)-ibot+1
  xbot<-y[ibot]
  xtop<-y[itop]
  winval<-ifelse(data<=xbot,xbot,data)
  ifelse(winval>=xtop,xtop,winval)
}