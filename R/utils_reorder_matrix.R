#' @importFrom stats as.dist hclust
#' @keywords internal
.reorder_matrix <- function(x, reorder_distance = NULL, method = "complete") {
  if (is.null(reorder_distance)) {
    reorder_distance <- x
    reorder_distance$Parameter <- NULL
    reorder_distance$Group <- NULL
  } else {
    reorder_distance$Parameter <- NULL
    reorder_distance$Group <- NULL
  }

  if (!isSquare(reorder_distance)) {
    stop("Matrix must be squared to be re-arranged.")
  }

  reorder_distance <- stats::as.dist((1 - reorder_distance) / 2, diag = TRUE, upper = TRUE)
  hc <- stats::hclust(reorder_distance, method = method)
  x <- x[hc$order, hc$order]
  x
}
