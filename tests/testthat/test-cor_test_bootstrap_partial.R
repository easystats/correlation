# Bootstrap intervals for partial correlations --------------------------------
#
# Each replicate takes the resampled raw rows and runs the chain of the point
# estimate: partial (x and y each regressed on every column other than x, y,
# and the cluster column), then winsorize, then rank transform, then the
# coefficient.
#
# Populations:
# - sim_partial(n): z1, z2, z3 ~ iid N(0, 1); x = 0.5 z1 + 0.3 z2 + e,
#   y = 0.4 z1 + 0.5 z3 + f, with (e, f) bivariate normal, unit variances and
#   correlation 0.4, independent of z. The partial correlation of x and y given
#   z1 to z3 is corr(e, f) = 0.4.
# - sim_partial_cluster(): 50 clusters of 12 rows; x_ij = u_i + e_ij + z_ij and
#   y_ij = u_i + f_ij + z_ij, with u, e, f, z ~ iid N(0, 1). Given z, x and y
#   are u + e and u + f, so the partial correlation is 1 / 2 = 0.5, and the
#   shared u makes rows of one cluster dependent.

sim_partial <- function(n) {
  z <- matrix(stats::rnorm(3 * n), n, 3)
  e <- stats::rnorm(n)
  f <- 0.4 * e + sqrt(1 - 0.4^2) * stats::rnorm(n)
  data.frame(
    x = 0.5 * z[, 1] + 0.3 * z[, 2] + e,
    y = 0.4 * z[, 1] + 0.5 * z[, 3] + f,
    z1 = z[, 1],
    z2 = z[, 2],
    z3 = z[, 3]
  )
}

sim_partial_cluster <- function(n_clusters = 50, size = 12) {
  id <- rep(seq_len(n_clusters), each = size)
  u <- stats::rnorm(n_clusters)[id]
  z <- stats::rnorm(length(id))
  data.frame(
    x = u + stats::rnorm(length(id)) + z,
    y = u + stats::rnorm(length(id)) + z,
    z = z,
    id = id
  )
}

covers <- function(rez, value) rez$CI_low <= value && value <= rez$CI_high

# AC1: plain bootstrap ---------------------------------------------------------

test_that("AC1: plain bootstrap covers the partial correlation 0.4 at n = 50", {
  skip_on_cran()
  set.seed(2026)
  hits <- vapply(
    seq_len(200),
    function(i) {
      rez <- cor_test(
        sim_partial(50),
        "x",
        "y",
        partial = TRUE,
        bootstrap = TRUE,
        iterations = 200,
        ci = 0.95,
        verbose = FALSE
      )
      covers(rez, 0.4)
    },
    TRUE
  )
  expect_gte(mean(hits), 0.88)
  expect_lte(mean(hits), 0.99)
})

test_that("AC1: bootstrap endpoints match the analytic endpoints at n = 2000", {
  skip_on_cran()
  set.seed(11)
  d <- sim_partial(2000)
  analytic <- cor_test(d, "x", "y", partial = TRUE, bootstrap = FALSE)
  set.seed(12)
  boot <- cor_test(
    d,
    "x",
    "y",
    partial = TRUE,
    bootstrap = TRUE,
    iterations = 2000
  )
  expect_identical(attr(boot, "ci_method"), "bootstrap")
  expect_lt(abs(boot$CI_low - analytic$CI_low), 0.006)
  expect_lt(abs(boot$CI_high - analytic$CI_high), 0.006)
})

# AC2: cluster bootstrap -------------------------------------------------------

test_that("AC2: cluster bootstrap covers the partial correlation 0.5", {
  skip_on_cran()
  set.seed(31)
  hits <- vapply(
    seq_len(200),
    function(i) {
      d <- sim_partial_cluster()
      boot <- cor_test(
        d,
        "x",
        "y",
        partial = TRUE,
        cluster = "id",
        iterations = 200,
        verbose = FALSE
      )
      analytic <- cor_test(d[c("x", "y", "z")], "x", "y", partial = TRUE)
      c(covers(boot, 0.5), covers(analytic, 0.5))
    },
    c(TRUE, TRUE)
  )
  expect_gte(mean(hits[1, ]), 0.86)
  expect_lte(mean(hits[1, ]), 0.99)
  expect_lt(mean(hits[2, ]), 0.85)
})

# AC3: the cluster column is never a covariate ---------------------------------

test_that("AC3: the cluster column does not change the partial coefficient", {
  set.seed(41)
  d <- sim_partial_cluster()
  no_id <- cor_test(d[c("x", "y", "z")], "x", "y", partial = TRUE)
  for (type in c("integer", "factor", "character")) {
    di <- d
    di$id <- switch(
      type,
      integer = as.integer(d$id),
      factor = factor(d$id),
      character = paste0("c", d$id)
    )
    rez <- cor_test(
      di,
      "x",
      "y",
      partial = TRUE,
      cluster = "id",
      iterations = 20
    )
    expect_equal(rez$r, no_id$r, tolerance = 1e-12, label = type)
    expect_identical(attr(rez, "ci_method"), "cluster-bootstrap")
  }
  # discriminating control: id entered as a covariate changes the coefficient
  with_id <- cor_test(d, "x", "y", partial = TRUE)
  expect_gt(abs(with_id$r - no_id$r), 1e-6)
})

test_that("AC3: cluster = row number reproduces the plain partial bootstrap", {
  set.seed(42)
  d <- sim_partial(60)
  d$id <- seq_len(nrow(d))
  set.seed(7)
  clustered <- cor_test(
    d,
    "x",
    "y",
    partial = TRUE,
    cluster = "id",
    iterations = 500
  )
  set.seed(7)
  plain <- cor_test(
    d[names(d) != "id"],
    "x",
    "y",
    partial = TRUE,
    bootstrap = TRUE,
    iterations = 500
  )
  expect_identical(
    attr(clustered, "bootstrap_replicates"),
    attr(plain, "bootstrap_replicates")
  )
})

# AC4: chain order inside a replicate --------------------------------------------

# the first resample of a cluster bootstrap, rebuilt by hand, then partialled
# with lm() residuals and transformed by `transform`
first_replicate_by_hand <- function(d, seed, transform) {
  set.seed(seed)
  groups <- split(seq_len(nrow(d)), d$id)
  idx <- sample.int(length(groups), length(groups), replace = TRUE)
  rd <- d[unlist(groups[idx], use.names = FALSE), ]
  rebuilt <- data.frame(
    x = stats::residuals(stats::lm(x ~ z, data = rd)),
    y = stats::residuals(stats::lm(y ~ z, data = rd))
  )
  cor_test(transform(rebuilt), "x", "y")$r
}

test_that("AC4: each replicate partials, then winsorizes or rank transforms", {
  set.seed(51)
  d <- sim_partial_cluster(n_clusters = 25, size = 8)
  # heavy tails so that winsorizing and ranking change the coefficient
  d$x <- d$x + stats::rt(nrow(d), df = 2)
  winsorized <- function(r) {
    as.data.frame(datawizard::winsorize(r, threshold = 0.2, verbose = FALSE))
  }
  ranked <- function(r) data.frame(x = rank(r$x), y = rank(r$y))

  set.seed(99)
  expect_silent(
    w <- cor_test(
      d,
      "x",
      "y",
      partial = TRUE,
      winsorize = 0.2,
      cluster = "id",
      iterations = 50
    )
  )
  expect_identical(attr(w, "iterations"), 50L)
  expect_equal(
    attr(w, "bootstrap_replicates")[1],
    first_replicate_by_hand(d, 99, winsorized),
    tolerance = 1e-12
  )

  set.seed(99)
  expect_silent(
    rk <- cor_test(
      d,
      "x",
      "y",
      partial = TRUE,
      ranktransform = TRUE,
      cluster = "id",
      iterations = 50
    )
  )
  expect_identical(attr(rk, "iterations"), 50L)
  expect_equal(
    attr(rk, "bootstrap_replicates")[1],
    first_replicate_by_hand(d, 99, ranked),
    tolerance = 1e-12
  )

  # discriminating control: winsorizing before partialling gives another value
  set.seed(99)
  groups <- split(seq_len(nrow(d)), d$id)
  idx <- sample.int(length(groups), length(groups), replace = TRUE)
  rd <- d[unlist(groups[idx], use.names = FALSE), ]
  rd[c("x", "y")] <- winsorized(rd[c("x", "y")])
  wrong_order <- cor(
    stats::residuals(stats::lm(x ~ z, data = rd)),
    stats::residuals(stats::lm(y ~ z, data = rd))
  )
  expect_gt(abs(attr(w, "bootstrap_replicates")[1] - wrong_order), 1e-6)

  # discriminating control: ranking before partialling gives another value
  rd <- d[unlist(groups[idx], use.names = FALSE), ]
  rd[c("x", "y")] <- ranked(rd[c("x", "y")])
  wrong_order <- cor(
    stats::residuals(stats::lm(x ~ z, data = rd)),
    stats::residuals(stats::lm(y ~ z, data = rd))
  )
  expect_gt(abs(attr(rk, "bootstrap_replicates")[1] - wrong_order), 1e-6)
})

test_that("each replicate re-partials instead of resampling fixed residuals", {
  set.seed(61)
  d <- sim_partial(40)
  fixed <- data.frame(
    x = stats::residuals(stats::lm(x ~ z1 + z2 + z3, data = d)),
    y = stats::residuals(stats::lm(y ~ z1 + z2 + z3, data = d))
  )
  set.seed(8)
  boot <- cor_test(
    d,
    "x",
    "y",
    partial = TRUE,
    bootstrap = TRUE,
    iterations = 100
  )
  # control: the same seed on the fixed residuals draws the same rows, so a
  # plain bootstrap of them matches resampling them by hand
  set.seed(8)
  by_hand <- vapply(
    seq_len(100),
    function(i) {
      rows <- sample.int(nrow(fixed), nrow(fixed), replace = TRUE)
      cor(fixed$x[rows], fixed$y[rows])
    },
    1
  )
  set.seed(8)
  boot_fixed <- cor_test(fixed, "x", "y", bootstrap = TRUE, iterations = 100)
  expect_equal(attr(boot_fixed, "bootstrap_replicates"), by_hand)
  # the partial bootstrap re-fits the regressions on the covariates in each
  # resample
  expect_false(isTRUE(all.equal(attr(boot, "bootstrap_replicates"), by_hand)))
  expect_equal(boot$r, cor(fixed$x, fixed$y), tolerance = 1e-12)
})

# AC5: still refused ----------------------------------------------------------------

test_that("AC5: bootstrap is refused for full Bayesian partial and multilevel", {
  d <- sim_partial(30)
  expect_error(
    cor_test(d, "x", "y", bootstrap = TRUE, partial_bayesian = TRUE),
    "not available for full Bayesian\\s+partial correlations"
  )
  expect_error(
    cor_test(d, "x", "y", bootstrap = TRUE, multilevel = TRUE),
    "not available for multilevel\\s+correlations"
  )
  # partial = TRUE alone is accepted
  expect_s3_class(
    cor_test(d, "x", "y", partial = TRUE, bootstrap = TRUE, iterations = 20),
    "easycor_test"
  )
})

# AC6: correlation() -------------------------------------------------------------

test_that("AC6: correlation() runs the partial cluster bootstrap for each pair", {
  set.seed(71)
  d <- sim_partial_cluster(n_clusters = 30, size = 6)
  d$w <- d$z + stats::rnorm(nrow(d))
  d <- d[c("x", "y", "w", "id")]
  no_id <- d[c("x", "y", "w")]

  set.seed(5)
  rez <- correlation(d, partial = TRUE, cluster = "id", iterations = 100)
  expect_identical(nrow(rez), 3L)
  expect_identical(
    paste(rez$Parameter1, rez$Parameter2),
    c("x y", "x w", "y w")
  )
  expect_identical(attr(rez, "ci_method"), "cluster-bootstrap")
  expect_equal(
    rez$r[1],
    correlation(no_id, partial = TRUE)$r[1],
    tolerance = 1e-12
  )

  set.seed(5)
  first <- cor_test(
    d,
    "x",
    "y",
    partial = TRUE,
    cluster = "id",
    iterations = 100
  )
  expect_equal(rez$CI_low[1], first$CI_low, tolerance = 1e-10)
  expect_equal(rez$CI_high[1], first$CI_high, tolerance = 1e-10)
  expect_equal(rez$SE[1], first$SE, tolerance = 1e-10)

  plain <- correlation(no_id, partial = TRUE, bootstrap = TRUE, iterations = 50)
  expect_identical(attr(plain, "ci_method"), "bootstrap")
})

test_that("AC5: correlation() refuses the bootstrap with partial_bayesian", {
  d <- sim_partial(30)
  expect_error(
    correlation(d, partial = TRUE, partial_bayesian = TRUE, bootstrap = TRUE),
    "not available for full Bayesian\\s+partial correlations"
  )
  d$id <- rep(1:10, 3)
  expect_error(
    correlation(d, partial_bayesian = TRUE, cluster = "id"),
    "not available for full Bayesian\\s+partial correlations"
  )
})

# Missing values and the resampled rows -----------------------------------------

# x and y given z1 to z3, each fitted on its own complete rows (as adjust()
# does: x without y, y without x), then correlated on the rows where both
# residuals exist
partial_by_hand <- function(d) {
  rx <- stats::residuals(
    stats::lm(x ~ z1 + z2 + z3, data = d, na.action = stats::na.exclude)
  )
  ry <- stats::residuals(
    stats::lm(y ~ z1 + z2 + z3, data = d, na.action = stats::na.exclude)
  )
  cor(rx, ry, use = "complete.obs")
}

# the replicates of resampling the rows of `d` by hand, after the same seed
rows_by_hand <- function(d, seed, iterations) {
  set.seed(seed)
  vapply(
    seq_len(iterations),
    function(i) {
      partial_by_hand(d[sample.int(nrow(d), nrow(d), replace = TRUE), ])
    },
    1
  )
}

# the same for the clusters of `d` in split() order
clusters_by_hand <- function(d, seed, iterations) {
  set.seed(seed)
  groups <- split(seq_len(nrow(d)), d$id)
  vapply(
    seq_len(iterations),
    function(i) {
      idx <- sample.int(length(groups), length(groups), replace = TRUE)
      partial_by_hand(d[unlist(groups[idx], use.names = FALSE), ])
    },
    1
  )
}

test_that("partial bootstrap resamples the rows that enter a regression", {
  set.seed(81)
  d <- sim_partial(40)
  d$x[3] <- NA # enters the regression of y
  d$y[7] <- NA # enters the regression of x
  d$z2[11] <- NA # enters neither regression
  d[15, c("x", "y")] <- NA # enters neither regression
  expect_equal(
    cor_test(d, "x", "y", partial = TRUE)$r,
    partial_by_hand(d),
    tolerance = 1e-12
  )

  set.seed(9)
  boot <- cor_test(
    d,
    "x",
    "y",
    partial = TRUE,
    bootstrap = TRUE,
    iterations = 5
  )
  replicates <- attr(boot, "bootstrap_replicates")
  expect_equal(
    replicates,
    rows_by_hand(d[-c(11, 15), ], 9, 5),
    tolerance = 1e-12
  )

  # discriminating controls: resampling every row, or only the rows complete
  # on all columns, draws other rows
  expect_gt(abs(replicates[1] - rows_by_hand(d, 9, 1)), 1e-6)
  complete <- d[stats::complete.cases(d), ]
  expect_gt(abs(replicates[1] - rows_by_hand(complete, 9, 1)), 1e-6)
})

test_that("partial cluster bootstrap resamples only clusters with a usable row", {
  set.seed(82)
  d <- sim_partial(75)
  d$id <- rep(1:25, each = 3)
  d$id[c(5, 6)] <- NA # dropped: no cluster
  d$z1[30] <- NA # dropped: enters neither regression
  d$z3[d$id %in% 25] <- NA # cluster 25 has no usable row
  d$x[d$id %in% 24] <- NA # cluster 24 enters only the regression of y
  set.seed(10)
  boot <- cor_test(
    d,
    "x",
    "y",
    partial = TRUE,
    cluster = "id",
    iterations = 5
  )
  with_id <- d[!is.na(d$id), ]
  expect_equal(boot$r, partial_by_hand(with_id), tolerance = 1e-12)
  used <- d[!is.na(d$id) & !is.na(d$z1) & !is.na(d$z3), ]
  expect_identical(length(unique(used$id)), 24L)
  expect_true(24 %in% used$id)
  replicates <- attr(boot, "bootstrap_replicates")
  expect_equal(replicates, clusters_by_hand(used, 10, 5), tolerance = 1e-12)

  # discriminating controls: keeping cluster 25 as a resampling unit, or
  # dropping cluster 24, draws other clusters
  with_25 <- d[!is.na(d$id) & !is.na(d$z1), ]
  expect_gt(abs(replicates[1] - clusters_by_hand(with_25, 10, 1)), 1e-6)
  without_24 <- used[used$id != 24, ]
  expect_gt(abs(replicates[1] - clusters_by_hand(without_24, 10, 1)), 1e-6)
})

test_that("partial cluster bootstrap counts the clusters that enter the coefficient", {
  set.seed(83)
  d <- sim_partial(66)
  d$id <- rep(1:22, each = 3)
  # cluster 22 has no row that enters a regression, and cluster 1 enters
  # only the regression of y: 20 clusters enter the coefficient
  d$z3[d$id == 22] <- NA
  d$x[d$id == 1] <- NA
  expect_silent(cor_test(
    d,
    "x",
    "y",
    partial = TRUE,
    cluster = "id",
    iterations = 5
  ))
  d$z3[d$id == 21] <- NA
  expect_warning(
    cor_test(d, "x", "y", partial = TRUE, cluster = "id", iterations = 5),
    "only 19 distinct values"
  )

  # a factor x is converted to numeric and partialled, so the count is taken
  # after the conversion and is the same
  d$x <- factor(round(d$x))
  expect_warning(
    cor_test(d, "x", "y", partial = TRUE, cluster = "id", iterations = 5),
    "only 19 distinct values"
  )

  # x missing in every row: no cluster enters the coefficient
  d$x <- NA_real_
  expect_error(
    cor_test(d, "x", "y", partial = TRUE, cluster = "id", iterations = 5),
    "at least 2 distinct values"
  )
})

test_that("correlation() warns for a pair with few clusters in the coefficient", {
  set.seed(85)
  d <- data.frame(
    a = stats::rnorm(90),
    b = stats::rnorm(90),
    c = stats::rnorm(90),
    id = rep(1:30, each = 3)
  )
  # a is missing in clusters 1 to 15: every pair has 15 clusters in its
  # coefficient (a is a variable of the first pair and a covariate of b-c)
  d$a[d$id <= 15] <- NA
  expect_warning(
    correlation(d, partial = TRUE, cluster = "id", iterations = 5),
    "only 15 distinct values"
  )
})

test_that("the partial rows: regressions, coefficient, and factors", {
  d <- data.frame(
    x = c(1, NA, 3, 4, 5, NA),
    y = c(1, 2, NA, 4, 5, NA),
    z = c(1, 2, 3, NA, 5, 6)
  )
  rows <- .partial_rows(d, "x", "y")
  # row 4 misses the covariate, row 6 both variables
  expect_identical(rows$coefficient, c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_identical(rows$used, c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE))

  # factors are not partialled: a factor needs only its own value, and a row
  # with a factor but no value of the other variable changes no estimate
  f <- d
  f$x <- factor(c("a", NA, "b", "a", "b", "a"))
  f$y <- factor(c("a", "b", NA, "b", "a", NA))
  rows <- .partial_rows(f, "x", "y")
  expect_identical(rows$coefficient, c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_identical(rows$used, rows$coefficient)
})

test_that("partialling fits the target variable whatever the column names", {
  # columns named like the internal arguments of the adjust step
  set.seed(84)
  d <- data.frame(
    a = stats::rnorm(50),
    b = stats::rnorm(50),
    x = stats::rnorm(50),
    var = stats::rnorm(50),
    data = stats::rnorm(50)
  )
  d$a <- d$a + d$x
  d$b <- d$b + d$var - d$data
  expected <- cor(
    stats::residuals(stats::lm(a ~ x + var + data, data = d)),
    stats::residuals(stats::lm(b ~ x + var + data, data = d))
  )
  expect_equal(
    cor_test(d, "a", "b", partial = TRUE)$r,
    expected,
    tolerance = 1e-12
  )
  expect_equal(
    cor_test(d, "x", "var", partial = TRUE)$r,
    cor(
      stats::residuals(stats::lm(x ~ a + b + data, data = d)),
      stats::residuals(stats::lm(var ~ a + b + data, data = d))
    ),
    tolerance = 1e-12
  )
})
