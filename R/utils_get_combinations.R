#' @keywords internal
.get_combinations <- function(data, data2 = NULL, redundant = TRUE, include_factors = TRUE, random = FALSE) {
  data <- .clean_data(data, include_factors = include_factors, random = random)
  data_nums <- data[sapply(data, is.numeric)]
  vars <- names(data_nums)

  # Find pairs
  if (is.null(data2)) {
    vars2 <- vars
  } else {
    data2 <- .clean_data(data2, include_factors = include_factors, random = random)
    data2_nums <- data2[sapply(data2, is.numeric)]
    vars2 <- names(data2_nums)
  }

  combinations <- expand.grid(vars, vars2, stringsAsFactors = FALSE)
  combinations <- combinations[order(match(combinations$Var1, vars), match(combinations$Var2, vars2)), ]

  row.names(combinations) <- NULL
  names(combinations) <- c("Parameter1", "Parameter2")

  if (redundant == FALSE) {
    combinations <- .remove_redundant(combinations)
  }

  combinations
}
