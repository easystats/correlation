#' @keywords internal
.find_correlationtype <- function(data, x, y) {
  type_x <- .vartype(data[[x]])
  type_y <- .vartype(data[[y]])

  if (type_x$is_binary && type_y$is_continuous) {
    if (type_x$is_factor) {
      method <- "biserial"
    } else {
      method <- "pointbiserial"
    }
  } else if (type_x$is_continuous && type_y$is_binary) {
    if (type_y$is_factor) {
      method <- "biserial"
    } else {
      method <- "pointbiserial"
    }
  } else if (type_x$is_binary && type_y$is_binary) {
    method <- "tetrachoric"
  } else if (type_x$is_factor || type_y$is_factor) {
    method <- "polychoric"
  } else {
    method <- "pearson"
  }
  method
}



#' @keywords internal
.vartype <- function(x) {
  out <- list(
    is_factor = FALSE,
    is_numeric = FALSE,
    is_character = FALSE,
    is_binary = FALSE,
    is_continuous = FALSE,
    is_count = FALSE
  )

  if (is.factor(x)) {
    out$is_factor <- TRUE
  }

  if (is.character(x)) {
    out$is_character <- TRUE
  }

  if (is.numeric(x)) {
    out$is_numeric <- TRUE
  }

  if (length(unique(x)) == 2) {
    out$is_binary <- TRUE
  }

  if (out$is_numeric && out$is_binary == FALSE) {
    out$is_continuous <- TRUE
  }

  if (all(x %% 1 == 0)) {
    out$is_count <- TRUE
  }

  out
}
