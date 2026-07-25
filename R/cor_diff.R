#' Test differences between correlations
#'
#' @description
#' Tests whether the correlation between two variables `x` and `y` is different
#' from the correlation between `x2` and `y2`.
#'
#' `cor_diff()` returns a table containing an index of difference precision (i.e.,
#' the estimated difference divided by its standard error) and an associated p-value.
#' A significant p-value indicates that the correlation between `x` and `y` is
#' different from the correlation between `x2` and `y2`.
#'
#' @param data A data frame of observations.
#' @param x,y,x2,y2 The variable names in `data` to be used. `x` and `y` can also
#' be pairs of variables, in which case the second variable is used as `x2` and `y2`.
#' @param method Can be `"parametric"` or `"bootstrapping"`. If `"parametric"`,
#' the [psych::r.test()] function is used. If `"bootstrapping"`, a bootstrapping
#' procedure is used.
#' @param ... Other arguments to be passed, for instance `iterations` (default: 1000)
#' if method is bootstrapping.
#'
#' @examples
#' cor_diff(iris, c("Sepal.Length", "Sepal.Width"), c("Sepal.Length", "Petal.Width"))
#' cor_diff(iris,
#'   c("Sepal.Length", "Sepal.Width"),
#'   c("Sepal.Length", "Petal.Width"),
#'   method = "bootstrapping", iterations = 100
#' )
#' @export
cor_diff <- function(data, x, y, x2 = NULL, y2 = NULL, method = "parametric", ...) {
  # If pairs are passed
  if (length(x) == 2 && length(y) == 2) {
    x2 <- y[1]
    y2 <- y[2]
    y <- x[2]
    x <- x[1]
  }

  # Compute
  if (method %in% c("bootstrapping")) {
    out <- .cor_diff_bootstrapping(data, x, y, x2, y2, ...)
  } else {
    out <- .cor_diff_parametric(data, x, y, x2, y2, ...)
  }
  class(out) <- c("cor_diff", class(out))
  out
}



# Methods -----------------------------------------------------------------



#' @keywords internal
.cor_diff_parametric <- function(data, x, y, x2, y2, ...) {
  insight::check_if_installed("psych", "for 'parametric' correlation difference method")

  args <- list(n = nrow(data), r12 = stats::cor(data[[x]], data[[y]]))
  if (x == x2 && y != y2) {
    args$r13 <- stats::cor(data[[x]], data[[y2]])
    args$r23 <- stats::cor(data[[y]], data[[y2]])
  } else if (y == y2 && x != x2) {
    args$r13 <- stats::cor(data[[y]], data[[x2]])
    args$r23 <- stats::cor(data[[x]], data[[x2]])
  } else {
    args$r34 <- stats::cor(data[[x2]], data[[y2]])
  }
  test <- do.call(psych::r.test, args)

  out <- data.frame(
    Method = "parametric"
  )
  if ("t" %in% names(test)) {
    out$t <- test$t
  } else {
    out$z <- test$z
  }
  out$p <- test$p
  out
}

#' @keywords internal
.cor_diff_bootstrapping <- function(data, x, y, x2, y2, iterations = 1000, robust = FALSE, ...) {
  diff <- rep(NA, iterations) # Initialize vector

  # Bootstrap
  for (i in 1:iterations) {
    # Take random sample of data
    dat <- data[sample(nrow(data), nrow(data), replace = TRUE), ]
    # Compute diff
    diff[i] <- stats::cor(dat[[x]], dat[[y]]) - stats::cor(dat[[x2]], dat[[y2]])
  }

  # Summarize
  if (robust == FALSE) {
    out <- data.frame(
      Method = "bootstrapping",
      z = mean(diff) / stats::sd(diff),
      p = bayestestR::pd_to_p(as.numeric(bayestestR::p_direction(diff)))
    )
  } else {
    out <- data.frame(
      Method = "bootstrapping_robust",
      z = stats::median(diff) / stats::mad(diff),
      p = bayestestR::pd_to_p(as.numeric(bayestestR::p_direction(diff)))
    )
  }
  out
}



# Printing ----------------------------------------------------------------

#' @export
print.cor_diff <- function(x, ...) {
  insight::format_table(x, ...) |>
    insight::export_table(title = "Correlation Difference Test") |>
    print()
  invisible(x)
}
