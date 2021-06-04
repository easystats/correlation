# target argument can be used to matrix-fy a specific column.
# @examples
# summary(correlation(mtcars), target = "p")
#' @export
summary.easycorrelation <- function(object,
                                    redundant = FALSE,
                                    target = NULL,
                                    ...) {

  # If data2 is present
  if (!is.null(attributes(object)$data2)) {
    redundant <- FALSE
  }

  frame <- .get_matrix(object, square = redundant)

  # Add redundant
  if (redundant) {
    object <- .add_redundant(object)
  }

  if (is.null(target)) {
    target <- names(object)[names(object) %in% c("r", "rho", "tau", "Median", "Dxy")][1]
    if (is.na(target)) {
      target <- names(object)[!names(object) %in% c("Parameter1", "Parameter2")][1]
    }
  } else {
    target <- target[target %in% names(object)][1]
    if (is.na(target) || length(target) == 0) {
      stop("`target` must be a column name in the correlation object.", call. = FALSE)
    }
  }

  out <- .create_matrix(frame, object, column = target, redundant = redundant)

  # Fill attributes
  for (i in names(object)[!names(object) %in% c("Group", "Parameter1", "Parameter2", target)]) {
    attri <- .create_matrix(frame, object, column = i, redundant = redundant)
    attr(out, i) <- attri
  }

  # Transfer attributes
  attributes(out) <- c(attributes(out), attributes(object)[!names(attributes(object)) %in% c("names", "row.names", "class", names(attributes(out)))])
  attributes(out) <- c(attributes(out), list(...))
  attr(out, "redundant") <- redundant
  attr(out, "coefficient_name") <- target

  if (inherits(object, "grouped_easycorrelation")) {
    class(out) <- c("easycormatrix", "see_easycormatrix", "grouped_easycormatrix", class(out))
  } else {
    class(out) <- c("easycormatrix", "see_easycormatrix", class(out))
  }

  out
}


#' @export
as.table.easycorrelation <- function(x, ...) {
  .Deprecated("summary(..., redundant = TRUE)")
  summary(x, redundant = TRUE)
}

# @examples
# as.matrix(correlation(mtcars))
#' @export
as.matrix.easycorrelation <- function(x, ...) {
  mat <- summary(x, redundant = TRUE)

  if (inherits(mat, "grouped_easycormatrix")) {
    mat$Parameter <- paste(mat$Group, "-", mat$Parameter)
    row.names(mat) <- mat$Parameter
    mat <- mat[-c(1:2)]
    # self-correlations
    mat[is.na(mat)] <- 1
  } else {
    row.names(mat) <- mat$Parameter
    mat <- mat[-1]
  }

  as.matrix(mat)
}

# @examples
# as.list(correlation(mtcars))
#' @export
as.list.easycorrelation <- function(x, cols = NULL, redundant = FALSE, ...) {
  if (inherits(x, "grouped_easycorrelation")) {
    lx <- split(x, x$Group)
    lx <- lapply(lx, .create_matrix_list, cols = cols)
  } else {
    lx <- .create_matrix_list(x, cols = cols)
  }

  if (inherits(x, "grouped_easycorrelation")) {
    class(lx) <- c("grouped_easymatrixlist", "easymatrixlist")
  }

  return(lx)
}


# plot ----------------------------

#' @export
plot.easycormatrix <- function(x, ...) {
  insight::check_if_installed("see", "to plot correlation graphs")

  NextMethod()
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


#' @keywords internal
.create_matrix_list <- function(object, cols = NULL, redundant = FALSE, ...) {
  if (inherits(object, "grouped_easycorrelation")) {
    class(object) <- class(object)[class(object) != "grouped_easycorrelation"]
  }

  if (is.null(cols)) {
    cols <- colnames(object)[!colnames(object) %in% c("Group", "Parameter1", "Parameter2")]
  }

  sx <- summary(object = object, redundant = redundant, ...)

  .corr <- sx
  attributes(.corr) <- attributes(.corr)[c("names", "row.names")]
  class(.corr) <- c("easycormatrix", "data.frame")

  lx <- c(
    list(coefficient = .corr),
    attributes(sx)[cols[cols %in% names(attributes(sx))]]
  )

  lx <- lapply(
    lx,
    `attributes<-`,
    value = attributes(.corr)
  )

  names(lx)[1] <- attributes(sx)$coefficient_name

  attributes(lx) <- c(
    attributes(lx),
    class = "easymatrixlist",
    attributes(sx)[!names(attributes(sx)) %in% c("names", "row.names", "class", "coefficient_name", names(lx))]
  )

  return(lx)
}
