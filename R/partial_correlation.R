#' @importFrom stats cov cov2cor p.adjust pnorm pt
#' @keywords internal
.partial_correlation <- function(data, method = "pearson", semi = FALSE) {

  # correlation method
  method <- match.arg(method, c("pearson", "kendall", "spearman"))

  x <- data
  # check the data
  if (is.data.frame(x)) {
    x <- x[sapply(x, is.numeric)]
  }
  x <- as.matrix(x)
  if (!is.matrix(x)) {
    stop("Data must be a matrix")
  }
  if (!(is.numeric(x) || is.logical(x))) {
    stop("Data must be numeric")
  }
  stopifnot(is.atomic(x))


  # sample number
  n <- dim(x)[1]

  # given variables' number
  gp <- dim(x)[2] - 2

  # covariance matrix
  cvx <- cov(x, method = method)

  # inverse covariance matrix
  if (det(cvx) < .Machine$double.eps) {
    warning("The inverse of variance-covariance matrix is calculated using Moore-Penrose generalized matrix invers due to its determinant of zero.")
    icvx <- generalized_inverse(cvx)
  } else {
    icvx <- solve(cvx)
  }

  # partial correlation
  if (semi == FALSE) {
    pcor <- -cov2cor(icvx)
  } else {
    pcor <- -cov2cor(icvx) / sqrt(diag(cvx)) / sqrt(abs(diag(icvx) - t(t(icvx^2) / diag(icvx))))
  }

  diag(pcor) <- 1

  # p-value
  if (method == "kendall") {
    statistic <- pcor / sqrt(2 * (2 * (n - gp) + 5) / (9 * (n - gp) * (n - 1 - gp)))
    diag(statistic) <- 0
    p.value <- 2 * pnorm(-abs(statistic))

    df <- .to_long_df(pcor, "tau")
    df <- merge(df, .to_long_df(statistic, "z"))
  } else {
    statistic <- pcor * sqrt((n - 2 - gp) / (1 - pcor^2))
    diag(statistic) <- 0
    p.value <- 2 * pt(-abs(statistic), (n - 2 - gp))

    if (method == "spearman") {
      df <- .to_long_df(pcor, "rho")
      df <- merge(df, .to_long_df(statistic, "S"))
    } else {
      df <- .to_long_df(pcor, "r")
      df <- merge(df, .to_long_df(statistic, "t"))
    }
  }

  diag(p.value) <- 0
  df <- merge(df, .to_long_df(p.value, "p"))

  return(df)
}




#' @keywords internal
.to_long_df <- function(m, name = "r") {
  df <- data.frame(row = rownames(m)[row(m)], col = colnames(m)[col(m)], corr = c(m))
  names(df) <- c("Parameter1", "Parameter2", name)
  return(df)
}
