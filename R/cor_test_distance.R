#' @keywords internal
.cor_test_distance <- function(data, x, y, ci = 0.95, corrected = TRUE, ...) {
  var_x <- .complete_variable_x(data, x, y)
  var_y <- .complete_variable_y(data, x, y)

  if (corrected == FALSE) {
    rez <- .cor_test_distance_raw(var_x, var_y, index = 1)
    rez <- data.frame(
      Parameter1 = x,
      Parameter2 = y,
      r = rez$r,
      CI_low = NA,
      CI_high = NA,
      t = NA,
      df_error = NA,
      p = NA,
      Method = "Distance"
    )
  } else {
    rez <- .cor_test_distance_corrected(var_x, var_y, ci = ci)
    rez <- data.frame(
      Parameter1 = x,
      Parameter2 = y,
      r = rez$r,
      CI_low = rez$CI_low,
      CI_high = rez$CI_high,
      t = rez$t,
      df_error = rez$df,
      p = rez$p,
      Method = "Distance (Bias Corrected)"
    )
  }

  rez
}




# Basis -------------------------------------------------------------------


#' @keywords internal
.cor_test_distance_corrected <- function(x, y, ci = 0.95) {
  x <- as.matrix(stats::dist(x))
  y <- as.matrix(stats::dist(y))
  n <- nrow(x)

  A <- .A_star(x)
  B <- .A_star(y)

  XY <- (sum(A * B) - (n / (n - 2)) * sum(diag(A * B))) / n^2
  XX <- (sum(A * A) - (n / (n - 2)) * sum(diag(A * A))) / n^2
  YY <- (sum(B * B) - (n / (n - 2)) * sum(diag(B * B))) / n^2

  r <- XY / sqrt(XX * YY)

  M <- n * (n - 3) / 2
  dof <- M - 1

  t <- sqrt(M - 1) * r / sqrt(1 - r^2)
  p <- 1 - stats::pt(t, df = dof)

  ci_vals <- cor_to_ci(r, n = n, ci = ci)

  list(
    r = r,
    t = t,
    df_error = dof,
    p = p,
    CI_low = ci_vals$CI_low,
    CI_high = ci_vals$CI_high
  )
}




#' @keywords internal
.cor_test_distance_raw <- function(x, y, index = 1) {
  if (index < 0 || index > 2) {
    stop("`index` must be between 0 and 2.")
    index <- 1.0
  }

  x <- as.matrix(stats::dist(x))
  y <- as.matrix(stats::dist(y))
  n <- nrow(x)

  A <- .A_kl(x, index)
  B <- .A_kl(y, index)

  cov <- sqrt(mean(A * B))
  dVarX <- sqrt(mean(A * A))
  dVarY <- sqrt(mean(B * B))
  V <- sqrt(dVarX * dVarY)
  if (V > 0) {
    r <- cov / V
  } else {
    r <- 0
  }
  list(r = r, cov = cov)
}





# Utils -------------------------------------------------------------------





#' @keywords internal
.A_kl <- function(x, index) {
  d <- as.matrix(x)^index
  m <- rowMeans(d)
  M <- mean(d)
  a <- sweep(d, 1, m)
  b <- sweep(a, 2, m)
  (b + M)
}


#' @keywords internal
.A_star <- function(d) {
  ## d is a distance matrix or distance object
  ## modified or corrected doubly centered distance matrices
  ## denoted A* (or B*) in JMVA t-test paper (2013)
  d <- as.matrix(d)
  n <- nrow(d)
  if (n != ncol(d)) stop("Argument d should be distance")
  m <- rowMeans(d)
  M <- mean(d)
  a <- sweep(d, 1, m)
  b <- sweep(a, 2, m)
  A <- b + M # same as plain A
  # correction to get A^*
  A <- A - d / n
  diag(A) <- m - M
  (n / (n - 1)) * A
}
