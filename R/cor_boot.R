cor_boot <- function(data,
                     x,
                     y,
                     method = "pearson",
                     ci = 0.95,
                     R = 2000,
                     cluster = NULL,
                     ...) {

  if (!is.null(cluster)) {
    if (!requireNamespace("tidyr", quietly = TRUE)) {
      stop("Package `tidyr` required for clustered bootstrapping. Please install it by running `install.packages('tidyr').", call. = FALSE)
    }
    bs_data <- data[, c(x, y, cluster)]
    bs_data <- tidyr::nest(bs_data, data = -c(cluster))
    # clustered bootstrap
    bs_results <- clusterboot(
      bs_data = bs_data,
      method = method,
      R = R,
      ...
    )
  } else {
    # nonclustered bootstrap
    bs_results <- singleboot(
      bs_data = data[, c(x, y)],
      method = method,
      R = R,
      ...
    )
  }

  bs_ci <- boot::boot.ci(bs_results, conf = ci, type = "bca", index = 2)

  out <- data.frame(
    "Parameter1" = x,
    "Parameter2" = y,
    r = bs_results$t0[2],
    se = sd(bs_results$t[, 2]),
    CI_low = bs_ci$bca[[4]],
    CI_high = bs_ci$bca[[5]],
    Method = method,
    stringsAsFactors = FALSE
  )

  out
}

## Non-clustered Bootstrap
singleboot <- function(bs_data, method, R, ...) {
  boot::boot(
    data = bs_data,
    statistic = singleboot_stat,
    R = R,
    method = method,
    ...
  )
}

singleboot_stat <- function(data, index, method) {
  stats::cor(data[index, ], method = method)
}

## Cluster Bootstrap
clusterboot <- function(bs_data, method, R, ...) {
  boot::boot(
    data = bs_data,
    statistic = clusterboot_stat,
    R = R,
    method = method,
    ...
  )
}

clusterboot_stat <- function(data, index, method) {
  resample <- data[index, ]
  resample <- tidyr::unnest(resample, cols = data)
  resample <- resample[-1]
  stats::cor(resample, method = method)
}


