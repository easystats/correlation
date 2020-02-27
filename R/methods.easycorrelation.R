#' @export
summary.easycorrelation <- function(object, redundant = FALSE, ...) {

  # If data2 is present
  if (!is.null(attributes(object)$data2)) {
    redundant <- FALSE
  }

  frame <- .get_matrix(object, square = redundant)


  # Add redundant
  if (redundant) {
    object <- .add_redundant(object)
  }

  target_col <- names(object)[names(object) %in% c("r", "rho", "tau", "Median")][1]
  if(is.na((target_col))){
    target_col <- names(object)[!names(object) %in% c("Parameter1", "Parameter2")][1]
  }
  out <- .create_matrix(frame, object, column = target_col, redundant = redundant)

  # Fill attributes
  for (i in names(object)[!names(object) %in% c("Group", "Parameter1", "Parameter2", target_col)]) {
    attri <- .create_matrix(frame, object, column = i, redundant = redundant)
    attr(out, i) <- attri
  }

  # Transfer attributes
  attributes(out) <- c(attributes(out), attributes(object)[!names(attributes(object)) %in% c("names", "row.names", "class", names(attributes(out)))])
  class(out) <- c("easycormatrix", class(out))
  out
}



#' @export
as.table.easycorrelation <- function(x, ...) {
  summary(x, redundant = TRUE)
}





#' @export
as.matrix.easycorrelation <- function(x, ...) {
  mat <- summary(x, redundant = TRUE)
  row.names(mat) <- mat$Parameter
  mat <- mat[-1]
  as.matrix(mat)
}



# Internals ---------------------------------------------------------------



#' @keywords internal
.create_matrix <- function(frame, object, column = "r", redundant = TRUE) {
  if ("Group" %in% names(object)) {
    out <- data.frame()
    for (g in unique(object$Group)) {
      data <- object[object$Group == g, ]
      m <- .fill_matrix(frame, data, column = column, redundant = redundant)
      m$Group <- g
      out <- rbind(out, m)
    }
    out <- out[c("Group", names(out)[names(out) != "Group"])]
  } else {
    out <- .fill_matrix(frame, object, column = column, redundant = redundant)
  }
  out
}














#' @keywords internal
.fill_matrix <- function(frame, object, column = "r", redundant = TRUE) {
  for (row in row.names(frame)) {
    for (col in colnames(frame)) {
      frame[row, col] <- object[(object$Parameter1 == row & object$Parameter2 == col) | (object$Parameter2 == row & object$Parameter1 == col), column][1]
    }
  }

  # Add Parameter column
  frame <- as.data.frame(frame)
  frame$Parameter <- row.names(frame)
  frame <- frame[c("Parameter", names(frame)[names(frame) != "Parameter"])]
  row.names(frame) <- NULL

  # Remove upper triangular
  if (redundant == FALSE & is.null(attributes(object)$data2)) {
    frame[-1][lower.tri(frame[-1])] <- NA
    frame <- frame[c(1, ncol(frame):2)]
  }

  frame
}
