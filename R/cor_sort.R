#' Sort a correlation matrix to improve readability of groups and clusters
#'
#' Sort a correlation matrix based on [`hclust()`].
#'
#' @param x A correlation matrix.
#' @param distance How the distance between each variable should be calculated.
#'   If `correlation` (default; suited for correlation matrices), the matrix
#'   will be rescaled to 0-1 (`distance = 0` indicating correlation of `1`;
#'   `distance = 1` indicating correlation of `-1`). If `raw`, then the matrix
#'   will be used as a distance matrix as-is. Can be others (`euclidean`,
#'   `manhattan`, ...), in which case it will be passed to [`dist()`] (see the
#'   arguments for it).
#' @param hclust_method Argument passed down into the `method` argument of [`hclust()`].
#' @param ... Other arguments to be passed to or from other functions.
#'
#' @examples
#' x <- correlation(mtcars)
#'
#' cor_sort(as.matrix(x))
#' cor_sort(x, hclust_method = "ward.D2") # It can also reorder the long form output
#' cor_sort(summary(x, redundant = TRUE)) # As well as from the summary
#' @export
cor_sort <- function(x, distance = "correlation", hclust_method = "complete", ...) {
  UseMethod("cor_sort")
}

#' @export
cor_sort.easycorrelation <- function(x, distance = "correlation", hclust_method = "complete", ...) {
  m <- cor_sort(as.matrix(x), distance = distance, hclust_method = hclust_method, ...)
  x$Parameter1 <- factor(x$Parameter1, levels = rownames(m))
  x$Parameter2 <- factor(x$Parameter2, levels = colnames(m))
  reordered <- x[order(x$Parameter1, x$Parameter2), ]

  # Restore class and attributes
  attributes(reordered) <- utils::modifyList(
    attributes(x)[!names(attributes(x)) %in% c("names", "row.names")],
    attributes(reordered)
  )

  # Make sure Parameter columns are character
  # Was added to fix a test, but makes the function not work
  # (See https://github.com/easystats/correlation/issues/259)
  # reordered$Parameter1 <- as.character(reordered$Parameter1)
  # reordered$Parameter2 <- as.character(reordered$Parameter2)

  reordered
}


#' @export
cor_sort.easycormatrix <- function(x, distance = "correlation", hclust_method = "complete", ...) {
  if (!"Parameter" %in% colnames(x)) {
    return(NextMethod())
  }

  # Get matrix
  m <- x
  row.names(m) <- x$Parameter
  m <- as.matrix(m[names(m)[names(m) != "Parameter"]])

  # If non-redundant matrix, fail (## TODO: fix that)
  if (anyNA(m)) {
    insight::format_error("Non-redundant matrices are not supported yet. Try again by setting summary(..., redundant = TRUE)")
  }

  # Get sorted matrix
  m <- cor_sort(m, distance = distance, hclust_method = hclust_method, ...)

  # Reorder
  x$Parameter <- factor(x$Parameter, levels = row.names(m))
  reordered <- x[order(x$Parameter), c("Parameter", colnames(m))]

  # Restore class and attributes
  attributes(reordered) <- utils::modifyList(
    attributes(x)[!names(attributes(x)) %in% c("names", "row.names")],
    attributes(reordered)
  )

  # Reorder attributes (p-values) etc.
  for (id in c("p", "CI", "CI_low", "CI_high", "BF", "Method", "n_Obs", "df_error", "t")) {
    if (id %in% names(attributes(reordered))) {
      attributes(reordered)[[id]] <- attributes(reordered)[[id]][order(x$Parameter), names(reordered)]
    }
  }

  # make sure Parameter columns are character
  reordered$Parameter <- as.character(reordered$Parameter)

  reordered
}


#' @export
cor_sort.matrix <- function(x, distance = "correlation", hclust_method = "complete", ...) {
  if (isSquare(x) && all(colnames(x) %in% rownames(x))) {
    i <- .cor_sort_square(x, distance = distance, hclust_method = hclust_method, ...)
  } else {
    i <- .cor_sort_nonsquare(x, distance = "euclidean", ...)
  }

  reordered <- x[i$row_order, i$col_order]

  # Restore class and attributes
  attributes(reordered) <- utils::modifyList(
    attributes(x)[names(attributes(x)) != "dimnames"],
    attributes(reordered)
  )

  reordered
}

# Utils -------------------------------------------------------------------


.cor_sort_square <- function(m, distance = "correlation", hclust_method = "complete", ...) {
  if (distance == "correlation") {
    d <- stats::as.dist((1 - m) / 2) # r = -1 -> d = 1; r = 1 -> d = 0
  } else if (distance == "raw") {
    d <- stats::as.dist(m)
  } else {
    d <- stats::dist(m, method = distance, diag = TRUE, upper = TRUE)
  }

  hc <- stats::hclust(d, method = hclust_method)
  row_order <- row.names(m)[hc$order]
  list(row_order = row_order, col_order = row_order)
}


.cor_sort_nonsquare <- function(m, distance = "euclidean", ...) {
  # Step 1: Perform clustering on rows and columns independently
  row_dist <- stats::dist(m, method = distance) # Distance between rows
  col_dist <- stats::dist(t(m), method = distance) # Distance between columns

  row_hclust <- stats::hclust(row_dist, method = "average")
  col_hclust <- stats::hclust(col_dist, method = "average")

  # Obtain clustering orders
  row_order <- row_hclust$order
  col_order <- col_hclust$order

  # Reorder matrix based on clustering
  clustered_matrix <- m[row_order, col_order]

  # Step 2: Refine alignment to emphasize strong correlations along the diagonal
  n_rows <- nrow(clustered_matrix)
  n_cols <- ncol(clustered_matrix)

  used_rows <- logical(n_rows)
  refined_row_order <- integer(0)

  for (col in seq_len(n_cols)) {
    max_value <- -Inf
    best_row <- NA

    for (row in seq_len(n_rows)[!used_rows]) {
      if (abs(clustered_matrix[row, col]) > max_value) {
        max_value <- abs(clustered_matrix[row, col])
        best_row <- row
      }
    }

    if (!is.na(best_row)) {
      refined_row_order <- c(refined_row_order, best_row)
      used_rows[best_row] <- TRUE
    }
  }

  # Append any unused rows at the end
  refined_row_order <- c(refined_row_order, which(!used_rows))

  # Apply
  m <- clustered_matrix[refined_row_order, ]
  list(row_order = rownames(m), col_order = colnames(m))
}
