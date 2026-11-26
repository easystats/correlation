#' @keywords internal
.clean_data <- function(
  data,
  include_factors = TRUE,
  multilevel = FALSE,
  keep_ordinal = FALSE
) {
  if (!multilevel) {
    if (include_factors) {
      # ordered factors become their level ranks, as in `cor_test()`, unless
      # `keep_ordinal`; unordered factors are dummy-coded
      keep <- keep_ordinal & vapply(data, .is_ordinal, logical(1))
      ordered_cols <- vapply(data, is.ordered, logical(1)) & !keep
      data[ordered_cols] <- lapply(data[ordered_cols], as.integer)
      if (any(keep)) {
        data <- do.call(
          cbind,
          lapply(names(data), function(i) {
            if (keep[[i]]) {
              data[i]
            } else {
              datawizard::to_numeric(data[i], dummy_factors = TRUE)
            }
          })
        )
      } else {
        data <- datawizard::to_numeric(data, dummy_factors = TRUE)
      }
    } else {
      data <- data[sapply(data, is.numeric)]
    }
  }
  data
}
