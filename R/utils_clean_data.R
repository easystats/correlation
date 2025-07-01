#' @keywords internal
.clean_data <- function(data, include_factors = TRUE, multilevel = FALSE) {
  if (!multilevel) {
    if (include_factors) {
      data <- datawizard::to_numeric(data, dummy_factors = TRUE)
    } else {
      data <- data[sapply(data, is.numeric)]
    }
  }
  data
}
