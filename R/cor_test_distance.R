#' # Test
#' # data <- data.frame("r" = c(), "n" = c(), "dCor" = c(), "Type" = c())
#' # for(r in seq(0, 0.99, length.out = 30)){
#' #   for(n in seq(10, 300, length.out = 30)){
#' #     x <- bayestestR::simulate_correlation(round(n), r = r)
#' #     data <- rbind(data,
#' #                   data.frame("r" = c(r, r),
#' #                              "n" = c(n, n),
#' #                              "dCor" = c(.cor_test_distance(x$V1, x$V2)$estimate,
#' #                                         .cor_test_distance_raw(x$V1, x$V2)$dCor),
#' #                              "Type" = c("Raw", "Corrected")))
#' #   }
#' # }
#' #
#' # ggplot(data, aes(x = r, y = dCor, color = Type)) +
#' #   geom_point(aes(size = n), alpha = 0.15) +
#' #   theme_classic()
#'
#'
#' #' @keywords internal
#' .cor_test_distance <- function(data, x, y, corrected = TRUE) {
#'
#'   var_x <- .complete_variable_x(data, x, y)
#'   var_y <- .complete_variable_y(data, x, y)
#'
#'   if(corrected == FALSE){
#'     rez <- .cor_test_distance_raw(var_x, var_y, index = 1)
#'     rez <- data.frame(Parameter1 = x,
#'                       Parameter2 = y,
#'                       r = rez$dCor,
#'                       Method = "Distance Correlation")
#'   } else{
#'     rez <- .cor_test_distance_correted(var_x, var_y)
#'     rez <- data.frame(Parameter1 = x,
#'                       Parameter2 = y,
#'                       r = rez$r,
#'                       t = rez$t,
#'                       df = rez$df,
#'                       p = rez$p,
#'                       Method = "Distance Correlation (Bias Corrected)")
#'   }
#'
#'
#'
#'
#' }
#'
#'
#'
#'
#' # Basis -------------------------------------------------------------------
#'
#'
#'
#' #' @keywords internal
#' .cor_test_distance_correted <- function(x, y, ci = 0.95) {
#'   x <- as.matrix(dist(x))
#'   y <- as.matrix(dist(y))
#'   n <- nrow(x)
#'
#'   A <- .A_star(x)
#'   B <- .A_star(y)
#'
#'   XY <- (sum(A * B) - (n / (n - 2)) * sum(diag(A * B))) / n^2
#'   XX <- (sum(A * A) - (n / (n - 2)) * sum(diag(A * A))) / n^2
#'   YY <- (sum(B * B) - (n / (n - 2)) * sum(diag(B * B))) / n^2
#'
#'   r <- XY / sqrt(XX * YY)
#'
#'   M <- n * (n - 3) / 2
#'   dof <- M - 1
#'
#'   t <- sqrt(M - 1) * r / sqrt(1 - r^2)
#'   p <- 1 - pt(tstat, df = df)
#'
#'   # http://onlinestatbook.com/chapter8/correlation_ci.html
#'   # error <- qt((1 - ci), df=df) * s / sqrt(n)
#'   # cor()
#'   # ci_low <- r-error
#'   # ci_high <- r+error
#'
#'   list(r = r, t = t, df = dof, p = p)
#' }
#'
#'
#'
#'
#' #' @keywords internal
#' .cor_test_distance_raw <- function(x, y, index = 1) {
#'   if (index < 0 || index > 2) {
#'     error("'index' must be between 0 and 2.")
#'     index <- 1.0
#'   }
#'
#'   x <- as.matrix(dist(x))
#'   y <- as.matrix(dist(y))
#'   n <- nrow(x)
#'
#'   A <- .A_kl(x, index)
#'   B <- .A_kl(y, index)
#'
#'   dCov <- sqrt(mean(A * B))
#'   dVarX <- sqrt(mean(A * A))
#'   dVarY <- sqrt(mean(B * B))
#'   V <- sqrt(dVarX * dVarY)
#'   if (V > 0) {
#'     dCor <- dCov / V
#'   } else {
#'     dCor <- 0
#'   }
#'   list(dCov = dCov, dCor = dCor)
#' }
#'
#'
#'
#'
#'
#' # Utils -------------------------------------------------------------------
#'
#'
#'
#'
#'
#' #' @keywords internal
#' .A_kl <- function(x, index) {
#'   d <- as.matrix(x)^index
#'   m <- rowMeans(d)
#'   M <- mean(d)
#'   a <- sweep(d, 1, m)
#'   b <- sweep(a, 2, m)
#'   return(b + M)
#' }
#'
#'
#' #' @keywords internal
#' .A_star <- function(d) {
#'   ## d is a distance matrix or distance object
#'   ## modified or corrected doubly centered distance matrices
#'   ## denoted A* (or B*) in JMVA t-test paper (2013)
#'   d <- as.matrix(d)
#'   n <- nrow(d)
#'   if (n != ncol(d)) stop("Argument d should be distance")
#'   m <- rowMeans(d)
#'   M <- mean(d)
#'   a <- sweep(d, 1, m)
#'   b <- sweep(a, 2, m)
#'   A <- b + M  #same as plain A
#'   #correction to get A^*
#'   A <- A - d/n
#'   diag(A) <- m - M
#'   (n / (n-1)) * A
#' }
#'
#'
#'
#'
#'
#'
