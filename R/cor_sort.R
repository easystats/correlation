#' Sort a correlation matrix to improve readability of groups and clusters
#'
#' Sort a correlation matrix based on `hclust`.
#'
#' @param x A correlation matrix.
#' @param distance How the distance between each variable should be calculated.
#'   If `correlation` (default; suited for correlation matrices), the matrix
#'   will be rescaled to 0-1 (`distance = 0` indicating correlation of `1`;
#'   `distance = 1` indicating correlation of `-1`). If `raw`, then the matrix
#'   will be used as a distance matrix as-is. Can be others (`euclidean`,
#'   `manhattan`, ...), in which case it will be passed to `dist()` (see the
#'   arguments for it).
#' @param ... Other arguments to be passed to or from other functions.
#'
#' @examples
#' x <- correlation(mtcars)
#'
#' cor_sort(as.matrix(x))
#' cor_sort(x, hclust_method = "ward.D2") # It can also reorder the long form output
#' cor_sort(summary(x, redundant = TRUE)) # As well as from the summary
#' @export
cor_sort <- function(x, distance = "correlation", ...) {
  UseMethod("cor_sort")
}

#' @export
cor_sort.easycorrelation <- function(x, distance = "correlation", ...) {
  order <- .cor_sort_order(as.matrix(x), distance = distance, ...)
  x$Parameter1 <- factor(x$Parameter1, levels = order)
  x$Parameter2 <- factor(x$Parameter2, levels = order)
  reordered <- x[order(x$Parameter1, x$Parameter2), ]

  # Restore class and attributes
  attributes(reordered) <- utils::modifyList(
    attributes(x)[!names(attributes(x)) %in% c("names", "row.names")],
    attributes(reordered)
  )

  # make sure Parameter columns are character
  reordered$Parameter1 <- as.character(reordered$Parameter1)
  reordered$Parameter2 <- as.character(reordered$Parameter2)

  reordered
}


#' @export
cor_sort.easycormatrix <- function(x, distance = "correlation", ...) {
  if (!"Parameter" %in% colnames(x)) {
    return(NextMethod())
  }

  # Get matrix
  m <- x
  row.names(m) <- x$Parameter
  m <- as.matrix(m[names(m)[names(m) != "Parameter"]])
  order <- .cor_sort_order(m, distance = distance, ...)

  # Reorder
  x$Parameter <- factor(x$Parameter, levels = order)
  reordered <- x[order(x$Parameter), c("Parameter", order)]

  # Restore class and attributes
  attributes(reordered) <- utils::modifyList(
    attributes(x)[!names(attributes(x)) %in% c("names", "row.names")],
    attributes(reordered)
  )

  # make sure Parameter columns are character
  reordered$Parameter <- as.character(reordered$Parameter)

  reordered
}


#' @export
cor_sort.matrix <- function(x, distance = "correlation", ...) {
  order <- .cor_sort_order(x, distance = distance, ...)
  reordered <- x[order, order]

  # Restore class and attributes
  attributes(reordered) <- utils::modifyList(
    attributes(x)[names(attributes(x)) != "dimnames"],
    attributes(reordered)
  )

  reordered
}

# Utils -------------------------------------------------------------------


.cor_sort_order <- function(m, distance = "correlation", hclust_method = "complete", ...) {
  if (distance == "correlation") {
    d <- stats::as.dist((1 - m) / 2) # r = -1 -> d = 1; r = 1 -> d = 0
  } else if (distance == "raw") {
    d <- stats::as.dist(m)
  } else {
    d <- stats::dist(m, method = distance, diag = TRUE, upper = TRUE)
  }

  hc <- stats::hclust(d, method = hclust_method)
  row.names(m)[hc$order]
}
