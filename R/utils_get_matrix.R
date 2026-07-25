#' @keywords internal
.get_matrix <- function(data, square = FALSE) {
  if (
    (all(data$Parameter1 %in% data$Parameter2) &&
      all(data$Parameter2 %in% data$Parameter1)) ||
      square
  ) {
    vars <- as.character(unique(c(data$Parameter1, data$Parameter2)))
    dim <- length(vars)
    m <- matrix(nrow = dim, ncol = dim, dimnames = list(vars, vars))
  } else {
    m <- matrix(
      nrow = length(unique(data$Parameter1)),
      ncol = length(unique(data$Parameter2)),
      dimnames = list(unique(data$Parameter1), unique(data$Parameter2))
    )
  }
  m[] <- 1
  m
}
