#' @keywords internal
.remove_redundant <- function(data) {

  if(all(data$Parameter1 %in% data$Parameter2) && all(data$Parameter2 %in% data$Parameter1)){
    m <- .get_matrix(data)
    m[upper.tri(m, diag = TRUE)] <- NA
    rows_NA <- .get_rows_non_NA(m)
    out <- data[!paste0(data$Parameter1, "_", data$Parameter2) %in% rows_NA, ]
  } else{
    # Might be some edgecases here
    out <- data
  }

  out <- out[out$Parameter1 != out$Parameter2, ]
  row.names(out) <- NULL
  out
}




#' @keywords internal
.get_matrix <- function(data){
  if(all(data$Parameter1 %in% data$Parameter2) && all(data$Parameter2 %in% data$Parameter1)){
    vars <- as.character(unique(data$Parameter1))
    dim <- length(vars)
    m <- matrix(nrow = dim, ncol = dim, dimnames = list(vars, vars))
  } else{
    m <- matrix(nrow = length(unique(data$Parameter1)),
                ncol = length(unique(data$Parameter2)),
                dimnames = list(unique(data$Parameter1), unique(data$Parameter2)))
  }
  m[] <- 1
  m
}




#' @keywords internal
.get_rows_non_NA <- function(m) {
  rows <- c()
  cols <- c()

  for (col in colnames(m)) {
    for (row in 1:nrow(m)) {
      if (!is.na(m[row, col])) {
        rows <- c(rows, row.names(m)[row])
        cols <- c(cols, col)
      }
    }
  }

  paste0(rows, "_", cols)
}