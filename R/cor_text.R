#' Correlation text
#'
#' This function returns a formatted character of correlation statistics.
#'
#' @param x A dataframe with correlation statistics.
#' @param show_ci,show_statistic,show_sig Toggle on/off different parts of the text.
#' @param ... Other arguments to be passed to or from other functions.
#'
#' @examples
#' rez <- cor_test(mtcars, "mpg", "wt")
#'
#' cor_text(rez)
#' cor_text(rez, show_statistic = FALSE, show_ci = FALSE, stars = TRUE)
#'
#' rez <- correlation(mtcars)
#'
#' cor_text(rez)
#' @export
cor_text <- function(x, show_ci = TRUE, show_statistic = TRUE, show_sig = TRUE, ...) {
  # Estimate
  candidates <- c("rho", "r", "tau", "Difference", "r_rank_biserial")
  estimate <- candidates[candidates %in% names(x)][1]
  out_text <- paste0(tolower(estimate), " = ", insight::format_value(x[[estimate]]))

  # CI
  if (show_ci && all(c("CI_high", "CI_low") %in% names(x))) {
    if (!is.null(attributes(x$conf.int)$conf.level)) {
      # htest
      out_text <- paste0(
        out_text,
        ", ",
        insight::format_ci(x$CI_low, x$CI_high, ci = attributes(x$conf.int)$conf.level)
      )
    } else if ("CI" %in% names(x)) {
      # param
      out_text <- paste0(
        out_text,
        ", ",
        insight::format_ci(x$CI_low, x$CI_high, ci = x$CI)
      )
    } else if ("ci" %in% names(attributes(x))) {
      # param
      out_text <- paste0(
        out_text,
        ", ",
        insight::format_ci(x$CI_low, x$CI_high, ci = attributes(x)$ci)
      )
    }
  }

  # Statistic
  if (show_statistic) {
    if ("t" %in% names(x)) {
      out_text <- paste0(
        out_text,
        ", t(",
        insight::format_value(x$df_error, protect_integers = TRUE),
        ") = ",
        insight::format_value(x$t)
      )
    } else if ("S" %in% names(x)) {
      out_text <- paste0(out_text, ", S = ", insight::format_value(x$S))
    } else if ("z" %in% names(x)) {
      out_text <- paste0(out_text, ", z = ", insight::format_value(table$z))
    } else if ("W" %in% names(x)) {
      out_text <- paste0("W = ", insight::format_value(x$W))
    } else if ("Chi2" %in% names(x)) {
      out_text <- paste0(out_text, ", Chi2 = ", insight::format_value(x$Chi2))
    }
  }

  # Significance
  if (show_sig) {
    if ("p" %in% names(x)) {
      out_text <- paste0(out_text, ", ", insight::format_p(x$p, digits = "apa", ...))
    } else if ("BF" %in% names(x)) {
      exact <- match.call()[["exact"]]
      if (is.null(exact)) exact <- TRUE
      out_text <- paste0(out_text, ", ", insight::format_bf(x$BF, exact = exact, ...))
    } else if ("pd" %in% names(x)) {
      out_text <- paste0(out_text, ", ", insight::format_pd(x$pd, ...))
    }
  }

  out_text
}
