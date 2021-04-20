cor_boot <- function(data,
                     x,
                     y,
                     method = "pearson",
                     ci = 0.95,
                     R = 2000,
                     cluster = NULL,
                     ...) {

  if (!is.null(cluster)) {
    bs_data <- data[, c(x, y, cluster)]
    bs_data <- split(bs_data, bs_data[[cluster]])
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

  # TODO: Replace this with the r-to-z transformation
  bs_ci <- boot::boot.ci(bs_results, conf = ci, type = "perc", index = 2)

  out <- data.frame(
    "Parameter1" = x,
    "Parameter2" = y,
    r = bs_results$t0[2],
    SE = sd(bs_results$t[, 2]),
    CI_low = bs_ci$percent[[4]],
    CI_high = bs_ci$percent[[5]],
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
  resample <- do.call(rbind, data[index])
  stats::cor(resample[, 1:2], method = method)
}


