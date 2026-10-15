#' @keywords internal
.clean_data <- function(data, include_factors = TRUE, multilevel = FALSE) {
  if (!multilevel) {
    if (include_factors) {
      # ordered factors become their level ranks, as in `cor_test()`;
      # unordered factors are dummy-coded
      ordered_cols <- vapply(data, is.ordered, logical(1))
      data[ordered_cols] <- lapply(data[ordered_cols], as.integer)
      data <- datawizard::to_numeric(data, dummy_factors = TRUE)
    } else {
      data <- data[sapply(data, is.numeric)]
    }
  }
  data
}
