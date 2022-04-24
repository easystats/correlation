#' Return the upper or lower triangular part
#'
#' Return the upper or lower triangular part of the correlation matrix.
#'
#' @param x A correlation object.
#' @param diag Should the diagonal be included?
#' @param ... Other arguments to be passed to or from other functions.
#'
#' @examples
#' x <- correlation(mtcars, redundant = TRUE)  # Generate full matrix
#' x <- cor_lower(x)
#'
#' if (require("ggplot2")) {
#'   ggplot(x, aes(x = Parameter2, y=Parameter1, fill=r)) +
#'     geom_tile()
#' }
#'
#' # Sorted
#' x <- correlation(mtcars, redundant = TRUE)  # Generate full matrix
#' x <- cor_sort(x)
#' x <- cor_lower(x)
#'
#' if (require("ggplot2")) {
#'   ggplot(x, aes(x = Parameter2, y=Parameter1, fill=r)) +
#'     geom_tile()
#' }
#' @export
cor_lower <- function(x, diag = FALSE, ...) {
  UseMethod("cor_lower")
}

#' @export
cor_lower.easycorrelation <- function(x, diag = FALSE, ...) {
  # Transform easycorrelation into matrix
  m <- as.matrix(x)
  m <- m[levels(as.factor(x$Parameter1)), levels(as.factor(x$Parameter2))]

  # Select upper triangular
  tri <- upper.tri(m, diag = diag)
  rownames(tri) <- rownames(m)
  colnames(tri) <- colnames(m)

  tokeep <- c()

  for(param1 in rownames(m)) {
    for(param2 in colnames(m)) {
      if(tri[param1, param2] == TRUE) {
        tokeep <- c(tokeep, which(x$Parameter1 == param1 & x$Parameter2 == param2))
      }
    }
  }

  x[tokeep, ]
}
