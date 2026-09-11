#' @keywords internal
.cor_test_bootstrap <- function(
  out,
  data,
  x,
  y,
  ci = 0.95,
  method = "pearson",
  cluster = NULL,
  iterations = 1000,
  verbose = TRUE,
  ...
) {
  # complete cases on the two variables and, if any, the cluster column
  cols <- unique(c(x, y, cluster))
  data <- data[stats::complete.cases(data[cols]), cols, drop = FALSE]
  n <- nrow(data)

  # resampling units: rows, or whole clusters in split() order
  if (is.null(cluster)) {
    groups <- NULL
    n_units <- n
  } else {
    groups <- split(seq_len(n), data[[cluster]])
    n_units <- length(groups)
  }

  # one replicate: recompute the coefficient on a resample; NA on failure
  .one_replicate <- function() {
    idx <- sample.int(n_units, n_units, replace = TRUE)
    rows <- if (is.null(groups)) idx else unlist(groups[idx], use.names = FALSE)
    rez <- tryCatch(
      suppressWarnings(suppressMessages(
        .cor_test_frequentist(
          data[rows, , drop = FALSE],
          x,
          y,
          ci = ci,
          method = method,
          ...
        )
      )),
      error = function(e) NULL
    )
    if (is.null(rez)) {
      return(NA_real_)
    }
    coef_name <- intersect(c("r", "rho", "tau", "Dxy"), names(rez))[1]
    as.numeric(rez[[coef_name]][1])
  }

  replicates <- vapply(seq_len(iterations), function(i) .one_replicate(), 1)
  kept <- replicates[!is.na(replicates)]
  n_kept <- length(kept)

  if (n_kept < iterations / 2) {
    insight::format_error(sprintf(
      "Only %d of the %d bootstrap replicates returned a coefficient (fewer than half). Check that the data support `method = \"%s\"` in resamples (for example, variables that are constant or lose a category when rows or clusters are resampled).",
      n_kept,
      iterations,
      method
    ))
  }
  if (n_kept < iterations && isTRUE(verbose)) {
    insight::format_warning(sprintf(
      "%d of the %d bootstrap replicates returned no coefficient and were dropped; %d replicates were kept.",
      iterations - n_kept,
      iterations,
      n_kept
    ))
  }

  # percentile interval, SE, and the percentile achieved significance level
  alpha <- (1 - ci) / 2
  ci_vals <- stats::quantile(kept, c(alpha, 1 - alpha), type = 7, names = FALSE)
  if (method %in% c("distance", "hoeffding")) {
    p <- NA_real_
  } else {
    p <- min(
      1,
      2 * min(sum(kept <= 0) + 1, sum(kept >= 0) + 1) / (n_kept + 1)
    )
  }

  # replace the analytic quantities in the point-estimate row
  out$CI_low <- ci_vals[1]
  out$CI_high <- ci_vals[2]
  out$SE <- stats::sd(kept)
  out$p <- p
  for (stat in intersect(c("t", "S", "z", "df_error"), names(out))) {
    out[[stat]] <- NA
  }

  attr(out, "ci_method") <- if (is.null(cluster)) {
    "bootstrap"
  } else {
    "cluster-bootstrap"
  }
  attr(out, "iterations") <- n_kept
  attr(out, "bootstrap_replicates") <- kept
  out
}
