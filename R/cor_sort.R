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
#' @param na_action How to deal with undefined correlations, i.e. `NA` values in
#'   the matrix (which typically arise when two variables were never observed
#'   together). Can be `"infer"` (default), `"zero"`, `"omit"` or `"error"`. See
#'   the *Undefined correlations* section below.
#' @param ... Other arguments to be passed to or from other functions.
#'
#' @section Undefined correlations:
#'
#' Clustering requires a complete distance matrix, so undefined correlations
#' have to be resolved before [`hclust()`] can be run. Note that this only
#' affects the *ordering*: the matrix that is returned keeps its `NA` values,
#' and no imputed value is ever reported as a coefficient.
#'
#' - `"infer"` (default) estimates each undefined correlation from the variables
#'   that were observed with both members of the pair, as
#'   \eqn{r_{bc} = r_{bS} R_{SS}^{-1} R_{Sc}}. This is the maximum-entropy
#'   (maximum-determinant) positive-definite completion of the matrix, i.e. the
#'   value that sets the partial correlation of the pair, given the remaining
#'   variables, to zero (Dempster, 1972). When no variable was observed with
#'   both members of the pair, or when the estimate cannot be computed, it falls
#'   back to `0` for that pair. With a single mediating variable it reduces to
#'   \eqn{r_{ab} \times r_{ac}}.
#' - `"zero"` treats every undefined correlation as `0`. Cheaper and fully
#'   transparent, but it asserts that the pair is unrelated, which can pull
#'   apart variables that the rest of the matrix suggests belong together
#'   (especially with `hclust_method = "complete"`).
#' - `"omit"` computes the ordering on the complete-case submatrix, and appends
#'   the variables involved in undefined pairs at the end, in their original
#'   order.
#' - `"error"` throws an error listing the undefined pairs.
#'
#' `"infer"` is only available for square correlation matrices (i.e., with
#' `distance = "correlation"`). For other distances, and for non-square
#' matrices, undefined distances are replaced by the average of the defined
#' ones unless `na_action = "error"`.
#'
#' @references
#' Dempster, A. P. (1972). Covariance selection. *Biometrics*, 28(1), 157-175.
#'
#' Grone, R., Johnson, C. R., Sá, E. M., & Wolkowicz, H. (1984). Positive
#' definite completions of partial Hermitian matrices. *Linear Algebra and its
#' Applications*, 58, 109-124.
#'
#' @examples
#' x <- correlation(mtcars)
#'
#' cor_sort(as.matrix(x))
#' cor_sort(x, hclust_method = "ward.D2") # It can also reorder the long form output
#' cor_sort(summary(x, redundant = TRUE)) # As well as from the summary
#'
#' # Matrices with undefined correlations (e.g., variables that were never
#' # observed together) can be sorted too
#' d <- data.frame(
#'   a = rnorm(50),
#'   b = c(rnorm(25), rep(NA, 25)),
#'   c = c(rep(NA, 25), rnorm(25))
#' )
#' rez <- correlation(d)
#' cor_sort(rez)
#' cor_sort(rez, na_action = "omit")
#' @export
cor_sort <- function(
  x,
  distance = "correlation",
  hclust_method = "complete",
  na_action = "infer",
  ...
) {
  UseMethod("cor_sort")
}

#' @export
cor_sort.easycorrelation <- function(
  x,
  distance = "correlation",
  hclust_method = "complete",
  na_action = "infer",
  ...
) {
  m <- cor_sort(
    as.matrix(x),
    distance = distance,
    hclust_method = hclust_method,
    na_action = na_action,
    ...
  )
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
cor_sort.easycormatrix <- function(
  x,
  distance = "correlation",
  hclust_method = "complete",
  na_action = "infer",
  ...
) {
  if (!"Parameter" %in% colnames(x)) {
    return(NextMethod())
  }

  # Get matrix
  m <- x
  row.names(m) <- x$Parameter
  m <- as.matrix(m[names(m)[names(m) != "Parameter"]])

  # If non-redundant matrix, fail (## TODO: fix that)
  # Note that this must not be inferred from the presence of NAs: a redundant
  # matrix legitimately contains NAs when two variables were never observed
  # together (see the na_action argument).
  if (.is_nonredundant_matrix(x, m)) {
    insight::format_error(
      "Non-redundant matrices are not supported yet. Try again by setting summary(..., redundant = TRUE)"
    )
  }

  # Get sorted matrix
  m <- cor_sort(
    m,
    distance = distance,
    hclust_method = hclust_method,
    na_action = na_action,
    ...
  )

  # Reorder
  x$Parameter <- factor(x$Parameter, levels = row.names(m))
  reordered <- x[order(x$Parameter), c("Parameter", colnames(m))]

  # Restore class and attributes
  attributes(reordered) <- utils::modifyList(
    attributes(x)[!names(attributes(x)) %in% c("names", "row.names")],
    attributes(reordered)
  )

  # Reorder attributes (p-values) etc.
  for (id in c(
    "p",
    "CI",
    "CI_low",
    "CI_high",
    "BF",
    "Method",
    "n_Obs",
    "df_error",
    "t"
  )) {
    if (id %in% names(attributes(reordered))) {
      attributes(reordered)[[id]] <- attributes(reordered)[[id]][
        order(x$Parameter),
        names(reordered)
      ]
    }
  }

  # make sure Parameter columns are character
  reordered$Parameter <- as.character(reordered$Parameter)

  reordered
}


#' @export
cor_sort.matrix <- function(
  x,
  distance = "correlation",
  hclust_method = "complete",
  na_action = "infer",
  ...
) {
  if (isSquare(x) && all(colnames(x) %in% rownames(x))) {
    i <- .cor_sort_square(
      x,
      distance = distance,
      hclust_method = hclust_method,
      na_action = na_action,
      ...
    )
  } else {
    i <- .cor_sort_nonsquare(
      x,
      distance = "euclidean",
      na_action = na_action,
      ...
    )
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

.cor_sort_square <- function(
  m,
  distance = "correlation",
  hclust_method = "complete",
  na_action = "infer",
  ...
) {
  na_action <- match.arg(na_action, c("infer", "zero", "omit", "error"))

  if (anyNA(m)) {
    if (na_action == "error") {
      insight::format_error(
        "The matrix contains undefined correlations, which cannot be clustered:",
        .cor_sort_undefined_pairs(m),
        "Use the `na_action` argument to specify how these should be handled."
      )
    }

    if (na_action == "omit") {
      return(.cor_sort_omit(
        m,
        distance = distance,
        hclust_method = hclust_method,
        ...
      ))
    }

    if (distance == "correlation") {
      m <- .complete_cormatrix(m, na_action = na_action)
    }
  }

  if (distance == "correlation") {
    d <- stats::as.dist((1 - m) / 2) # r = -1 -> d = 1; r = 1 -> d = 0
  } else if (distance == "raw") {
    d <- stats::as.dist(m)
  } else {
    d <- stats::dist(m, method = distance, diag = TRUE, upper = TRUE)
  }

  # Safety net: distances that are still undefined (e.g. dist() on rows that
  # share no complete observation) are set to the average defined distance.
  d <- .cor_sort_complete_dist(d)

  hc <- stats::hclust(d, method = hclust_method)
  row_order <- row.names(m)[hc$order]
  list(row_order = row_order, col_order = row_order)
}


# Cluster the complete-case submatrix, then append the left-out variables.
.cor_sort_omit <- function(
  m,
  distance = "correlation",
  hclust_method = "complete",
  ...
) {
  keep <- !apply(is.na(m) | is.na(t(m)), 1, any)

  if (sum(keep) < 2L) {
    insight::format_warning(
      "Fewer than two variables are free of undefined correlations, so the matrix cannot be sorted. Returning it unsorted.",
      "Use a different `na_action` to sort it anyway."
    )
    return(list(row_order = row.names(m), col_order = row.names(m)))
  }

  sorted <- .cor_sort_square(
    m[keep, keep, drop = FALSE],
    distance = distance,
    hclust_method = hclust_method,
    na_action = "zero",
    ...
  )

  # Appended in their original order
  row_order <- c(sorted$row_order, row.names(m)[!keep])
  list(row_order = row_order, col_order = row_order)
}


# Maximum-entropy (zero partial correlation) completion of a correlation matrix.
# See Dempster (1972) on covariance selection, and Grone et al. (1984) on
# positive definite completions.
.complete_cormatrix <- function(
  m,
  na_action = "infer",
  tolerance = 1e-6,
  iterations = 100L
) {
  observed <- m

  # A value known in only one triangle is a valid observation for both
  transposed <- t(m)
  fill <- is.na(observed) & !is.na(transposed)
  observed[fill] <- transposed[fill]

  # Self-correlations
  diagonal <- diag(observed)
  diagonal[is.na(diagonal)] <- 1
  diag(observed) <- diagonal

  undefined <- which(is.na(observed) & upper.tri(observed), arr.ind = TRUE)
  if (nrow(undefined) == 0L) {
    return(observed)
  }

  # Start from the neutral value; this is the answer for na_action = "zero"
  out <- observed
  out[is.na(out)] <- 0
  if (na_action == "zero") {
    return(out)
  }

  # Estimates depend on each other when several pairs are undefined, so cycle
  # until the completion stabilises
  for (i in seq_len(iterations)) {
    previous <- out
    for (pair in seq_len(nrow(undefined))) {
      row <- undefined[pair, 1]
      col <- undefined[pair, 2]
      estimate <- .complete_cormatrix_pair(observed, out, row, col)
      out[row, col] <- estimate
      out[col, row] <- estimate
    }
    if (max(abs(out - previous)) < tolerance) {
      break
    }
  }

  out
}


# Estimate one undefined correlation from the variables observed with both
# members of the pair: r_ij = r_iS %*% solve(R_SS) %*% R_Sj
.complete_cormatrix_pair <- function(observed, m, row, col, ridge = 1e-8) {
  s <- setdiff(seq_len(nrow(m)), c(row, col))
  # Only variables actually observed with *both* members carry information
  s <- s[!is.na(observed[row, s]) & !is.na(observed[col, s])]

  if (length(s) == 0L) {
    return(0)
  }

  # Ridge-regularised, as the submatrix can be singular (collinear variables)
  covariances <- m[s, s, drop = FALSE]
  diag(covariances) <- diag(covariances) + ridge
  inverse <- try(solve(covariances), silent = TRUE)
  if (inherits(inverse, "try-error")) {
    return(0)
  }

  estimate <- as.vector(
    m[row, s, drop = FALSE] %*% inverse %*% m[s, col, drop = FALSE]
  )

  if (!is.finite(estimate)) {
    return(0)
  }

  # Cauchy-Schwarz keeps this within [-1, 1] for well-formed input, but the
  # ridge and the iteration can nudge it out
  max(-1, min(1, estimate))
}


# Replace undefined distances by the average defined distance
.cor_sort_complete_dist <- function(d) {
  undefined <- !is.finite(d)
  if (!any(undefined)) {
    return(d)
  }
  d[undefined] <- if (all(undefined)) 1 else mean(d[!undefined])
  d
}


.cor_sort_undefined_pairs <- function(m) {
  missing <- is.na(m) | is.na(t(m))
  undefined <- which(missing & upper.tri(missing), arr.ind = TRUE)
  if (nrow(undefined) == 0L) {
    return(character(0))
  }
  paste0(
    row.names(m)[undefined[, 1]],
    " - ",
    colnames(m)[undefined[, 2]],
    collapse = ", "
  )
}


# A non-redundant (triangular) matrix is one where half of the matrix has been
# masked out. This must be distinguished from a redundant matrix that happens to
# contain undefined correlations: those are missing in *both* triangles, whereas
# masking removes only one of the two.
.is_nonredundant_matrix <- function(x, m) {
  # Cross-correlation matrices (two datasets) are never redundant, but are
  # supported nonetheless
  if (!is.null(attributes(x)$data2)) {
    return(FALSE)
  }

  redundant <- attributes(x)$redundant
  if (!is.null(redundant)) {
    return(!isTRUE(redundant))
  }

  # Fall back on the structure of the matrix itself
  if (!isSquare(m) || !all(colnames(m) %in% rownames(m))) {
    return(FALSE)
  }
  m <- m[colnames(m), colnames(m), drop = FALSE]
  any(is.na(m) != is.na(t(m)))
}


.cor_sort_nonsquare <- function(
  m,
  distance = "euclidean",
  na_action = "infer",
  ...
) {
  na_action <- match.arg(na_action, c("infer", "zero", "omit", "error"))

  if (anyNA(m) && na_action == "error") {
    insight::format_error(
      "The matrix contains undefined correlations, which cannot be clustered.",
      "Use the `na_action` argument to specify how these should be handled."
    )
  }

  # Step 1: Perform clustering on rows and columns independently
  row_dist <- stats::dist(m, method = distance) # Distance between rows
  col_dist <- stats::dist(t(m), method = distance) # Distance between columns

  row_hclust <- stats::hclust(
    .cor_sort_complete_dist(row_dist),
    method = "average"
  )
  col_hclust <- stats::hclust(
    .cor_sort_complete_dist(col_dist),
    method = "average"
  )

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
      # isTRUE() so that undefined cells are simply skipped
      if (isTRUE(abs(clustered_matrix[row, col]) > max_value)) {
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
