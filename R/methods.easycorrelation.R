#' @rdname correlation
#'
#' @param lower Remove the upper triangular part of the matrix.
#' @export
summary.easycorrelation <- function(object, lower = TRUE){

  if ("Group" %in% names(object)) {
    datalist <- split(object, object$Group)
    m <- list()
    for (group in names(datalist)) {
      m[[group]] <- .summary_easycorrelation(datalist[[group]], lower = lower)
    }
    out <- do.call(rbind, m)
    row.names(out) <- NULL
    for(i in names(attributes(m[[group]]))){
      if(!i %in% c("names", "row.names", "class")){
        attri <- list()
        for (group in names(datalist)) {
          attri[[group]] <- attributes(m[[group]])[[i]]
        }
        attr(out, i) <- do.call(rbind, attri)
        row.names(attr(out, i)) <- NULL
      }
    }

  } else{
    out <- .summary_easycorrelation(object, lower = lower)
  }

  class(out) <- c("easycormatrix", class(out))
  out
}



#' @export
as.table.easycorrelation <- function(x) {
  summary(x, lower = FALSE)
}






# Internals ---------------------------------------------------------------


#' @keywords internal
.summary_easycorrelation <- function(object, lower = TRUE){

  frame <- .get_matrix(object, square = !lower)

  # Add redundant
  if(lower == FALSE){
    object <- .add_redundant(object)
  }

  r_col <- names(object)[names(object) %in% c("r", "rho", "Median")][1]
  out <- .fill_matrix(frame, object, column = r_col)

  # If grouped
  if("Group" %in% names(object)){
    out$Group <- unique(object$Group)[1]
    out <- out[c("Group", names(out)[names(out) != "Group"])]
  }


  for(i in names(object)[!names(object) %in% c("Group", "Parameter1", "Parameter2", r_col)]){
    attri <- .fill_matrix(frame, object, column = i)
    if("Group" %in% names(object)){
      attri$Group <- unique(object$Group)[1]
      attri <- attri[c("Group", names(attri)[names(attri) != "Group"])]
    }
    attr(out, i) <- attri
  }

  out
}




#' @keywords internal
.fill_matrix <- function(frame, object, column = "r"){
  for(row in row.names(frame)){
    for(col in colnames(frame)){
      frame[row, col] <- object[(object$Parameter1 == row & object$Parameter2 == col) | (object$Parameter2 == row & object$Parameter1 == col), column][1]
    }
  }

  # Add Parameter column
  frame <- as.data.frame(frame)
  frame$Parameter <- row.names(frame)
  frame <- frame[c("Parameter", names(frame)[names(frame) != "Parameter"])]
  row.names(frame) <- NULL

  frame
}









#' @rdname correlation
#'
#' @param object Object of class \link{correlation}.
#' @param which_column Which column to use for the matrix.
#' @param reorder Reorder the matrix based on correlation pattern (currently only works with square matrices)?
#' @param reorder_method Reordering method. See \link{hclust}.
#' @param reorder_distance The distance matrix used for reordering.
#' @param lower Remove the upper triangular part of the matrix.
#'
#' @export
# summary.easycorrelation <- function(object, which_column = NULL, reorder = TRUE, reorder_method = "complete", lower = TRUE, reorder_distance = NULL, ...) {
#   x <- object
#
#   if (is.null(which_column)) {
#     if ("r" %in% names(x)) {
#       which_column <- "r"
#     } else if ("Median" %in% names(x)) {
#       which_column <- "Median"
#     } else {
#       stop("Please specify `which_column`.")
#     }
#   }
# #
#   if ("Group" %in% names(x)) {
#     datalist <- split(x, x$Group)
#     m <- data.frame()
#     for (group in names(datalist)) {
#       dat <- datalist[[group]]
#       dat$Group <- NULL
#       dat <- .create_matrix(dat, which_column, reorder = FALSE, reorder_method = reorder_method, lower = lower, reorder_distance = reorder_distance)
#       dat$Group <- group
#       dat[nrow(dat) + 1, ] <- NA
#       m <- rbind(m, dat)
#     }
#     m <- m[-nrow(m), ]
#   } else {
#     m <- .create_matrix(x, which_column, reorder = reorder, reorder_method = reorder_method, lower = lower, reorder_distance = reorder_distance)
#   }
#
#   # Reorder columns
#   if ("Group" %in% names(m)) {
#     m <- m[c("Group", "Parameter", names(m)[!names(m) %in% c("Group", "Parameter")])]
#   } else {
#     m <- m[c("Parameter", names(m)[!names(m) %in% c("Parameter")])]
#   }
#
#   # Remove empty
#   m <- m[, colSums(is.na(m)) < nrow(m)]
#   m <- m[rowSums(is.na(m)) != ncol(m[!names(m) %in% c("Parameter", "Group")]), ]
#
#   # Reset row names
#   row.names(m) <- NULL
#
#   m
# }





#'
#' #' @keywords internal
#' .create_matrix <- function(x, which_column, reorder = TRUE, reorder_method = "complete", lower = TRUE, reorder_distance = NULL) {
#'   rows <- unique(as.character(x$Parameter1))
#'   cols <- unique(as.character(x$Parameter2))
#'   m <- data.frame(matrix(ncol = length(cols), nrow = length(rows)), row.names = rows)
#'   colnames(m) <- cols
#'
#'   for (col in cols) {
#'     for (row in rows) {
#'       if (row == col) {
#'         cell <- NA
#'       } else {
#'         if (is.null(x[x$Parameter1 == row & x$Parameter2 == col, which_column])) {
#'           cell <- NA
#'         } else {
#'           cell <- x[x$Parameter1 == row & x$Parameter2 == col, which_column]
#'           if (length(cell) == 0) {
#'             cell <- x[x$Parameter1 == col & x$Parameter2 == row, which_column]
#'           }
#'         }
#'       }
#'       m[row, col] <- cell
#'     }
#'   }
#'
#'
#'   if (reorder == TRUE & all(unique(rownames(m)) == unique(names(m)))) {
#'     m <- .reorder_matrix(m, reorder_distance = reorder_distance, method = reorder_method)
#'   }
#'
#'   # Remove upper
#'   if (lower == TRUE & all(unique(rownames(m)) == unique(names(m)))) {
#'     m[upper.tri(m, diag = TRUE)] <- NA
#'   }
#'
#'   m$Parameter <- row.names(m)
#'   row.names(m) <- NULL
#'
#'   m
#' }
#'
#'
#'
#'
#'
#'
#'
#' #' @importFrom stats as.dist hclust
#' #' @keywords internal
#' .reorder_matrix <- function(x, reorder_distance = NULL, method = "complete") {
#'   if (is.null(reorder_distance)) {
#'     reorder_distance <- x
#'     reorder_distance$Parameter <- NULL
#'     reorder_distance$Group <- NULL
#'   } else {
#'     reorder_distance$Parameter <- NULL
#'     reorder_distance$Group <- NULL
#'   }
#'
#'   if (ncol(x) != nrow(x) | ncol(reorder_distance) != nrow(reorder_distance)) {
#'     warning("Matrix must be squared to be re-arranged.")
#'     return(x)
#'   }
#'
#'   reorder_distance <- stats::as.dist((1 - reorder_distance) / 2, diag = TRUE, upper = TRUE)
#'   hc <- stats::hclust(reorder_distance, method = method)
#'   x <- x[hc$order, hc$order]
#'   return(x)
#' }
