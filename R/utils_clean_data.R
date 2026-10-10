#' @keywords internal
.clean_data <- function(
  data,
  include_factors = TRUE,
  multilevel = FALSE,
  keep_ordinal = FALSE
) {
  if (!multilevel) {
    if (include_factors) {
      # with `keep_ordinal`, ordered factors of three or more levels stay
      # factors (for `method = "auto"`); other factors are dummy-coded
      keep <- keep_ordinal & vapply(data, .is_ordinal, logical(1))
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
