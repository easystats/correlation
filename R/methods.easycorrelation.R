#' @export
summary.easycorrelation <- function(object, ...){
  summary(as.data.frame(object))
}





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



#' Create correlation table
#'
#' @param x Object of class \link{correlation}.
#' @param which_column Which column to use for the matrix.
#' @param reorder Reorder the matrix based on correlation pattern (currently only works with square matrices)?
#' @param reorder_method Reordering method. See \link{hclust}.
#' @param reorder_distance The distance matrix used for reordering.
#' @param lower Remove the upper triangular part of the matrix.
#' @param ... Arguments passed to or from other methods.
#'
#' @export
as.table.easycorrelation <- function(x, which_column=NULL, reorder=TRUE, reorder_method="complete", lower=TRUE, reorder_distance=NULL, ...) {
  if(is.null(which_column)){
    if("r" %in% names(x)){
      which_column <- "r"
    } else if("Median" %in% names(x)){
      which_column <- "Median"
    } else{
      stop("Please specify `which_column`.")
    }
  }

  if("Group" %in% names(x)){
    datalist <- split(x, x$Group)
    m <- data.frame()
    for(group in names(datalist)){
      dat <- datalist[[group]]
      dat$Group <- NULL
      dat <- .create_matrix(dat, which_column, reorder=FALSE, reorder_method=reorder_method, lower=lower, reorder_distance = reorder_distance)
      dat$Group <- group
      dat[nrow(dat) + 1, ] <- NA
      m <- rbind(m, dat)
    }
    m <- m[-nrow(m),]
  } else{
    m <- .create_matrix(x, which_column, reorder=reorder, reorder_method=reorder_method, lower=lower, reorder_distance = reorder_distance)
  }

  # Reorder columns
  if("Group" %in% names(m)){
    m <- m[c('Group', 'Parameter', names(m)[!names(m) %in% c('Group', 'Parameter')])]
  } else{
    m <- m[c('Parameter', names(m)[!names(m) %in% c('Parameter')])]
  }

  # Remove empty
  m <- m[,colSums(is.na(m))<nrow(m)]
  m <- m[rowSums(is.na(m)) != ncol(m[!names(m) %in% c("Parameter", "Group")]),]

  # Reset row names
  row.names(m) <- NULL


  return(m)

}




#' @keywords internal
.create_matrix <- function(x, which_column, reorder=TRUE, reorder_method="complete", lower=TRUE, reorder_distance = NULL){
  rows <- sort(unique(x$Parameter1))
  cols <- sort(unique(x$Parameter2))
  m <- data.frame(matrix(ncol=length(cols), nrow=length(rows)), row.names = rows)
  colnames(m) <- cols

  for(col in cols){
    for(row in rows){
      if(is.null(x[x$Parameter1 == row & x$Parameter2 == col, which_column])){
        cell <- NA
      } else{
        cell <- x[x$Parameter1 == row & x$Parameter2 == col, which_column]
      }
      m[row, col] <- cell
    }
  }


  if (reorder == TRUE & all(unique(rownames(m)) == unique(names(m)))) {
    m <- .reorder_matrix(m, reorder_distance = reorder_distance, method=reorder_method)
  }

  # Remove upper
  if(lower==TRUE & all(unique(rownames(m)) == unique(names(m)))){
    m[upper.tri(m, diag = TRUE)] <- NA
  }

  m$Parameter <- row.names(m)
  row.names(m) <- NULL

  return(m)
}







#' @importFrom stats as.dist hclust
#' @keywords internal
.reorder_matrix <- function(x, reorder_distance = NULL, method="complete") {
  if (is.null(reorder_distance)) {
    reorder_distance <- x
    reorder_distance$Parameter <- NULL
    reorder_distance$Group <- NULL
  } else{
    reorder_distance$Parameter <- NULL
    reorder_distance$Group <- NULL
  }

  if (ncol(x) != nrow(x) | ncol(reorder_distance) != nrow(reorder_distance)) {
    warning("Matrix must be squared to be re-arranged.")
    return(x)
  }

  reorder_distance <- stats::as.dist((1 - reorder_distance) / 2, diag = TRUE, upper = TRUE)
  hc <- stats::hclust(reorder_distance, method = method)
  x <- x[hc$order, hc$order]
  return(x)
}