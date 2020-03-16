#' Fisher z-transformation
#'
#' The Fisher z-transformation converts the standard Pearson's r to a normally distributed variable z'. It is used to compute confidence intervals to correlations. The z' variable is different from the z-statistic.
#'
#' @param r,z The r or the z' value to be converted.
#' 
#' @return The transformed value.
#'
#' @examples
#' z_fisher(r = 0.7)
#' z_fisher(z = 0.867)
#' @references Zar, J.H., (2014). Spearman Rank Correlation: Overview. Wiley StatsRef: Statistics Reference Online. doi:10.1002/9781118445112.stat05964
#'
#' @export
z_fisher <- function(r = NULL, z = NULL) {
  # TODO: add variants for Spearman and Kendall (Zar, 2014)
  if (is.null(z)) {
    return(atanh(r))
  } else {
    return(tanh(z))
  }
}
