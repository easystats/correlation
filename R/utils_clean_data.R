#' @keywords internal
.clean_data <- function(data, include_factors = TRUE, multilevel = FALSE) {
  if (multilevel == FALSE) {
    if (include_factors) {
      data <- datawizard::to_numeric(data)
    } else {
      data <- data[sapply(data, is.numeric)]
    }
  }
  data
}
