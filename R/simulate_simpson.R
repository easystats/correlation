#' Simpson's paradox dataset simulation
#'
#' Simpson's paradox, or the Yule-Simpson effect, is a phenomenon in probability and statistics, in which a trend appears in several different groups of data but disappears or reverses when these groups are combined.
#'
#' @inheritParams bayestestR::simulate_correlation
#' @param n The number of observations for each group to be generated.
#' @param groups Number of groups.
#' @param difference Difference between groups.
#'
#' @return A dataset.
#'
#' @examples
#' data <- simulate_simpson(n = 100, groups = 5, r = 0.5)
#'
#' library(ggplot2)
#' ggplot(data, aes(x = V1, y = V2)) +
#'   geom_point(aes(color = Group)) +
#'   geom_smooth(aes(color = Group), method = "lm") +
#'   geom_smooth(method = "lm")
#' @export
simulate_simpson <- function(n = 100, r = 0.5, groups = 3, difference = 1) {
  data <- data.frame()
  for (i in 1:groups) {
    dat <- bayestestR::simulate_correlation(n = n, r = r)
    dat$V1 <- dat$V1 + difference * i # (i * -sign(r))
    dat$V2 <- dat$V2 + difference * (i * -sign(r))
    dat$Group <- LETTERS[i]
    data <- rbind(data, dat)
  }
  data
}
