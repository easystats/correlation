#' A Pseudo ANOVA test to test if correlations are similar
#'
#' Based on https://gist.github.com/sTeamTraen/eea2d7754f39653cd1884e29df84953c
#' Created by Nick Brown
#'
#' @param object A correlation object.
#' @param ... Currently not used.
#'
#' @examples
#' r <- correlation(mtcars)
#' anova(r)
#'
#' @export
anova.easycorrelation <- function(object, ...) {
  z_vals <- correlation::z_fisher(r=object[[attributes(object)$coefficient_name]])
  n <- object$n_Obs

  tot1 <- 0
  tot2 <- 0
  tot3 <- 0
  for (i in 1:length(z_vals)) {
    z <- z_vals[i]
    nm3 <- n[i] - 3
    tot1 <- tot1 + (nm3 * (z ^ 2))
    tot2 <- tot2 + (nm3 * z)
    tot3 <- tot3 + nm3
  }

  chisq <- tot1 - ((tot2 ^ 2) / tot3)
  df <- length(z_vals) - 1
  p <- stats::pchisq(chisq, df)

  out <- data.frame(Chi2 = chisq, df = df, p = p)
  attr(out, "table_title") <- "Test that the correlations are similar."
  class(out) <- c("easycorrelation_anova", class(out))
  out
}

print.easycorrelation_anova <- function(x, ...) {
  insight::export_table(insight::format_table(x))
}
