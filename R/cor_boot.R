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

  bs_ci <- boot::boot.ci(bs_results, conf = ci, type = "perc", index = 2)
  bs_ci <- bs_ci$percent[4:5]

  out <- data.frame(
    "Parameter1" = x,
    "Parameter2" = y,
    r = z_to_r(bs_results$t0[2]),
    SE = sd(z_to_r(bs_results$t[, 2])),
    CI_low = z_to_r(bs_ci[[1]]),
    CI_high = z_to_r(bs_ci[[2]]),
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

r_to_z <- function(r) {
  0.5 * log((1 + r) / (1 - r))
}

z_to_r <- function(z) {
  (exp(2 * z) - 1) / (exp(2 * z) + 1)
}

singleboot_stat <- function(data, index, method) {
  r <- stats::cor(data[index, ], method = method)
  z <- r_to_z(r)
  z
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
  r <- stats::cor(resample[, 1:2], method = method)
  z <- r_to_z(r)
  z
}


