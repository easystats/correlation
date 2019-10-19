#' @keywords internal
.remove_redundant <- function(params) {

  if(all(params$Parameter1 %in% params$Parameter2) && all(params$Parameter2 %in% params$Parameter1)){
    m <- .get_matrix(params)
    m[upper.tri(m, diag = TRUE)] <- NA
    rows_NA <- .get_rows_non_NA(m)
    out <- params[!paste0(params$Parameter1, "_", params$Parameter2) %in% rows_NA, ]
  } else{
    # Might be some edgecases here
    out <- params
  }

  out <- out[out$Parameter1 != out$Parameter2, ]
  row.names(out) <- NULL
  out
}





#' @keywords internal
.add_redundant <- function(params, data = NULL){
  inversed <- params
  inversed[, c("Parameter1", "Parameter2")] <- params[, c("Parameter2", "Parameter1")]
  params <- rbind(params, inversed)
  params <- rbind(params, .create_diagonal(params))

  # Reorder
  if(!is.null(data)){
    params <- params[order(match(params$Parameter1, names(data)), match(params$Parameter2, names(data))), ]
  }

  params
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