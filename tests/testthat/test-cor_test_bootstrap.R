# Bootstrap confidence intervals, standard errors, and p-values ---------------
#
# Oracle records (one line per oracle: id | type | asserting test | source and
# provenance). The asserting test is the record of the value; nothing here
# restates a number a test does not assert.
#
# O1  | simulation-coverage | "AC1: plain bootstrap covers rho = 0.5 at n = 50"
#       | data simulated from a bivariate normal with rho = 0.5; bounds
#       [0.88, 0.99] give a correct implementation a pass probability of 0.997
#       at its measured coverage 0.93 (cairn/reviews/archive/RR01 section 6).
# O2  | closed-form | "AC1: bootstrap endpoints match Fisher-z endpoints at n = 2000"
#       | Fisher-z interval from stats::cor.test(); tolerance 0.006 covers the
#       measured maximum endpoint difference 0.0043 over 100 seeds (RR01 s. 4).
# O3  | simulation-coverage | "AC2: cluster bootstrap covers rho = 0.5 with 50 clusters"
#       | x_ij = u_i + e_ij, y_ij = u_i + f_ij, u, e, f ~ N(0, 1): marginal
#       correlation 0.5, intraclass correlation 0.5; the Fisher interval on the
#       same data has coverage about 0.71 (RR01 section 5).
# O4  | invariant | "AC2: cluster = row number reproduces the plain bootstrap"
#       | sample.int(G, G, TRUE) over split() order makes both draws identical.
# O5  | closed-form | "AC6: SE and p are recomputed from the replicates"
#       | SE = sd of kept replicates (Efron & Tibshirani 1993 eq. 6.6); p in the
#       (count + 1) / (B + 1) form (Davison & Hinkley 1997 section 4.2.1).
# O6  | simulation-coverage | "AC6: type-I error at rho = 0 is near nominal"
#       | expected rejection rate 0.065 at n = 50 for the percentile p under
#       bivariate normality (RR01 section 1 table); bounds [0.02, 0.10].
# O7  | closed-form | "AC6: bootstrap p agrees with cor_to_p() at n = 2000"
#       | dataset constructed with sample correlation exactly 0.04 so that the
#       analytic p is about 0.074; measured maximum difference 0.031 (RR01 s. 6).
# O8  | closed-form | "AC6: SE agrees with (1 - r^2) / sqrt(n) at n = 2000"
#       | large-sample sd of Pearson r under bivariate normality.
# O9  | frozen (published) | "AC6: law-school SE matches Efron & Tibshirani"
#       | Efron & Tibshirani (1993) Table 3.1 (the 15 law schools' LSAT and
#       GPA) and Table 6.1 (bootstrap SE 0.132 at B = 3200); transcribed from
#       RR01 pending a read of the shelved PDF at T5 (observed 2026-09-10);
#       the sample correlation 0.776 is checked as a transcription control.
# O10 | invariant | "AC6: sign flip and affine map of y", the definition's
#       invariances: negating y negates and swaps the endpoints and keeps SE
#       and p; 2 * y + 5 changes nothing for the listed methods.
# O11 | invariant | "coherence: p and the interval agree on excluding 0"
#       | (p < 1 - ci) == (CI excludes 0) except within 4 / (B + 1) of the
#       threshold, where type-7 interpolation can disagree (RR01 section 6).
# O12 | simulation-coverage | "negative control: y = x^2 + noise"
#       | dependent but uncorrelated data: the bootstrap tests coefficient = 0
#       (rate measured 0.069) while the Fisher test tests independence under
#       normality (rate measured 0.290) (RR01 section 1).

# Helpers ---------------------------------------------------------------------

sim_bvn <- function(n, rho) {
  u <- stats::rnorm(n)
  data.frame(x = u, y = rho * u + sqrt(1 - rho^2) * stats::rnorm(n))
}

sim_cluster <- function(n_clusters = 50, size = 12) {
  g <- rep(seq_len(n_clusters), each = size)
  u <- stats::rnorm(n_clusters)[g]
  data.frame(
    g = g,
    x = u + stats::rnorm(length(g)),
    y = u + stats::rnorm(length(g))
  )
}

# Efron & Tibshirani (1993), Table 3.1: LSAT and GPA of 15 law schools
law_school <- data.frame(
  LSAT = c(
    576,
    635,
    558,
    578,
    666,
    580,
    555,
    661,
    651,
    605,
    653,
    575,
    545,
    572,
    594
  ),
  GPA = c(
    3.39,
    3.30,
    2.81,
    3.03,
    3.44,
    3.07,
    3.00,
    3.43,
    3.36,
    3.13,
    3.12,
    2.74,
    2.76,
    2.88,
    2.96
  )
)

# iris-derived columns for the method sweep: dichotomised and ordinal columns
# have no category below 30% of rows
iris_boot <- function() {
  d <- iris[c("Sepal.Length", "Petal.Length", "Sepal.Width")]
  d$bin_x <- as.numeric(iris$Sepal.Length > 5.8)
  d$bin_y <- as.numeric(iris$Petal.Length > 4.35)
  d$ord_x <- cut(
    iris$Petal.Length,
    c(-Inf, 1.9, 4.9, Inf),
    ordered_result = TRUE
  )
  d$ord_y <- cut(
    iris$Sepal.Width,
    c(-Inf, 2.8, 3.1, Inf),
    ordered_result = TRUE
  )
  d
}

# one row per method: x, y, coefficient column, Suggests package, slow legs
method_sweep <- list(
  list(method = "pearson", x = "Sepal.Length", y = "Petal.Length", coef = "r"),
  list(
    method = "spearman",
    x = "Sepal.Length",
    y = "Petal.Length",
    coef = "rho"
  ),
  list(
    method = "kendall",
    x = "Sepal.Length",
    y = "Petal.Length",
    coef = "tau"
  ),
  list(
    method = "somers",
    x = "bin_x",
    y = "Petal.Length",
    coef = "Dxy",
    pkg = "Hmisc"
  ),
  list(method = "biweight", x = "Sepal.Length", y = "Petal.Length", coef = "r"),
  list(method = "distance", x = "Sepal.Length", y = "Petal.Length", coef = "r"),
  list(
    method = "percentage",
    x = "Sepal.Length",
    y = "Petal.Length",
    coef = "r"
  ),
  list(
    method = "blomqvist",
    x = "Sepal.Length",
    y = "Petal.Length",
    coef = "r",
    pkg = "wdm"
  ),
  list(
    method = "hoeffding",
    x = "Sepal.Length",
    y = "Petal.Length",
    coef = "r",
    pkg = "Hmisc",
    slow = TRUE
  ),
  list(method = "gamma", x = "Sepal.Length", y = "Petal.Length", coef = "r"),
  list(method = "gaussian", x = "Sepal.Length", y = "Petal.Length", coef = "r"),
  list(
    method = "shepherd",
    x = "Sepal.Length",
    y = "Petal.Length",
    coef = "rho",
    slow = TRUE
  ),
  list(method = "biserial", x = "Petal.Length", y = "bin_x", coef = "rho"),
  list(method = "pointbiserial", x = "Petal.Length", y = "bin_x", coef = "rho"),
  list(
    method = "polychoric",
    x = "ord_x",
    y = "ord_y",
    coef = "rho",
    pkg = "psych"
  ),
  list(
    method = "polychoric",
    x = "ord_x",
    y = "Sepal.Length",
    coef = "rho",
    pkg = c("psych", "polycor"),
    label = "polyserial"
  ),
  list(
    method = "tetrachoric",
    x = "bin_x",
    y = "bin_y",
    coef = "rho",
    pkg = "psych"
  )
)
for (i in seq_along(method_sweep)) {
  if (is.null(method_sweep[[i]]$label)) {
    method_sweep[[i]]$label <- method_sweep[[i]]$method
  }
}

skip_sweep_entry <- function(spec) {
  if (isTRUE(spec$slow)) {
    skip_on_cran()
  }
  for (p in spec$pkg) {
    skip_if_not_installed(p)
  }
}

# the methods whose coefficient is odd under y -> -y and invariant under an
# affine map of y; the invariance sweep runs on integer-valued iris columns
# (tenths of a unit) so that 2 * y + 5 is exact in floating point
odd_methods <- c(
  "pearson",
  "spearman",
  "kendall",
  "somers",
  "gamma",
  "blomqvist",
  "biweight",
  "percentage",
  "gaussian"
)
iris_integer <- function() {
  d <- iris_boot()
  d$Sepal.Length <- round(d$Sepal.Length * 10)
  d$Petal.Length <- round(d$Petal.Length * 10)
  d
}

# a bootstrap call with the RNG reseeded immediately before it
seeded <- function(seed, expr) {
  set.seed(seed)
  expr
}

# collect the warnings a call raises, returning the value and their messages
with_warnings <- function(expr) {
  msgs <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = msgs)
}

# AC1 -------------------------------------------------------------------------

test_that("AC1: plain bootstrap covers rho = 0.5 at n = 50", {
  skip_on_cran()
  set.seed(101)
  covered <- vapply(
    seq_len(200),
    function(i) {
      d <- sim_bvn(50, 0.5)
      out <- cor_test(
        d,
        "x",
        "y",
        bootstrap = TRUE,
        iterations = 200,
        ci = 0.95
      )
      out$CI_low <= 0.5 && out$CI_high >= 0.5
    },
    logical(1)
  )
  expect_gte(mean(covered), 0.88)
  expect_lte(mean(covered), 0.99)
})

test_that("AC1: bootstrap endpoints match Fisher-z endpoints at n = 2000", {
  skip_on_cran()
  set.seed(102)
  d <- sim_bvn(2000, 0.5)
  boot <- seeded(
    103,
    cor_test(d, "x", "y", bootstrap = TRUE, iterations = 2000)
  )
  fisher <- cor_test(d, "x", "y", bootstrap = FALSE)
  expect_lt(abs(boot$CI_low - fisher$CI_low), 0.006)
  expect_lt(abs(boot$CI_high - fisher$CI_high), 0.006)

  # O8: SE against the large-sample sd of r
  r <- boot$r
  expect_lt(abs(boot$SE - (1 - r^2) / sqrt(2000)), 0.002)
})

# AC2 -------------------------------------------------------------------------

test_that("AC2: cluster bootstrap covers rho = 0.5 with 50 clusters", {
  skip_on_cran()
  set.seed(201)
  res <- vapply(
    seq_len(200),
    function(i) {
      d <- sim_cluster(50, 12)
      clus <- cor_test(d, "x", "y", cluster = "g", iterations = 200)
      fish <- cor_test(d, "x", "y", bootstrap = FALSE)
      c(
        cluster = clus$CI_low <= 0.5 && clus$CI_high >= 0.5,
        fisher = fish$CI_low <= 0.5 && fish$CI_high >= 0.5
      )
    },
    c(cluster = NA, fisher = NA)
  )
  expect_gte(mean(res["cluster", ]), 0.88)
  expect_lte(mean(res["cluster", ]), 0.99)
  expect_lt(mean(res["fisher", ]), 0.85)
})

test_that("AC2: cluster = row number reproduces the plain bootstrap", {
  d <- iris
  d$row <- seq_len(nrow(d))
  clus <- seeded(
    202,
    cor_test(
      d,
      "Sepal.Length",
      "Petal.Length",
      cluster = "row",
      iterations = 2000
    )
  )
  plain <- seeded(
    202,
    cor_test(
      d,
      "Sepal.Length",
      "Petal.Length",
      bootstrap = TRUE,
      iterations = 2000
    )
  )
  expect_identical(
    attr(clus, "bootstrap_replicates"),
    attr(plain, "bootstrap_replicates")
  )
  expect_identical(attr(clus, "ci_method"), "cluster-bootstrap")
  expect_identical(attr(plain, "ci_method"), "bootstrap")
})

# AC4: dropped replicates -------------------------------------------------------

test_that("AC4: NA replicates are dropped with one warning naming the kept count", {
  set.seed(401)
  for (constant in c("x", "y")) {
    d <- data.frame(
      g = rep(1:2, each = 20),
      x = stats::rnorm(40),
      y = stats::rnorm(40)
    )
    d[[constant]][d$g == 1] <- 5
    res <- with_warnings(cor_test(
      d,
      "x",
      "y",
      cluster = "g",
      iterations = 1000
    ))
    expect_length(res$warnings, 2L)
    expect_match(res$warnings, "only 2 distinct values", all = FALSE)
    dropped <- grep("replicates were kept", res$warnings, value = TRUE)
    expect_length(dropped, 1L)
    kept <- attr(res$value, "iterations")
    expect_gte(kept, 700)
    expect_lte(kept, 800)
    expect_match(dropped, sprintf("%d replicates were kept", kept))
    expect_identical(kept, length(attr(res$value, "bootstrap_replicates")))
    expect_true(all(is.finite(attr(res$value, "bootstrap_replicates"))))
  }
})

test_that("AC4: more than half failing replicates is an error naming the counts", {
  original <- .cor_test_freq
  calls <- 0L
  local_mocked_bindings(
    .cor_test_freq = function(...) {
      calls <<- calls + 1L
      # the first call is the point estimate; then 3 of every 5 calls error
      if (calls > 1L && ((calls - 2L) %% 5L) < 3L) {
        stop("mocked helper failure")
      }
      original(...)
    }
  )
  expect_error(
    cor_test(
      iris,
      "Sepal.Length",
      "Petal.Length",
      bootstrap = TRUE,
      iterations = 1000
    ),
    "Only 400 of the 1000 bootstrap replicates"
  )
})

test_that("AC4: fewer than half failing replicates warns with the kept count", {
  original <- .cor_test_freq
  calls <- 0L
  local_mocked_bindings(
    .cor_test_freq = function(...) {
      calls <<- calls + 1L
      # the first call is the point estimate; then 2 of every 5 calls error
      if (calls > 1L && ((calls - 2L) %% 5L) < 2L) {
        stop("mocked helper failure")
      }
      original(...)
    }
  )
  expect_warning(
    out <- cor_test(
      iris,
      "Sepal.Length",
      "Petal.Length",
      bootstrap = TRUE,
      iterations = 1000
    ),
    "600 replicates were kept"
  )
  expect_identical(attr(out, "iterations"), 600L)
})

test_that("AC4: no failing replicate raises no dropped-replicate warning", {
  expect_no_warning(
    cor_test(
      iris,
      "Sepal.Length",
      "Petal.Length",
      bootstrap = TRUE,
      iterations = 50
    )
  )
})

test_that("AC4: verbose = FALSE suppresses the cluster and dropped-replicate warnings", {
  set.seed(402)
  d <- data.frame(
    g = rep(1:2, each = 20),
    x = stats::rnorm(40),
    y = stats::rnorm(40)
  )
  d$x[d$g == 1] <- 5
  expect_no_warning(
    out <- cor_test(
      d,
      "x",
      "y",
      cluster = "g",
      iterations = 200,
      verbose = FALSE
    )
  )
  expect_lt(attr(out, "iterations"), 200L)
})

# AC6 -------------------------------------------------------------------------

for (spec in method_sweep) {
  test_that(
    paste("AC6: SE and p are recomputed from the replicates:", spec$label),
    {
      skip_sweep_entry(spec)
      d <- iris_boot()
      out <- seeded(
        601,
        cor_test(
          d,
          spec$x,
          spec$y,
          method = spec$method,
          bootstrap = TRUE,
          iterations = 50
        )
      )
      if (spec$method %in% c("distance", "hoeffding")) {
        expect_identical(out$p, NA_real_)
      } else {
        t <- attr(out, "bootstrap_replicates")
        B <- length(t)
        expect_equal(out$SE, stats::sd(t))
        expect_equal(
          out$p,
          min(1, 2 * min(sum(t <= 0) + 1, sum(t >= 0) + 1) / (B + 1))
        )
      }
    }
  )
}

test_that("AC6: type-I error at rho = 0 is near nominal", {
  skip_on_cran()
  set.seed(602)
  rejected <- vapply(
    seq_len(200),
    function(i) {
      d <- sim_bvn(50, 0)
      cor_test(d, "x", "y", bootstrap = TRUE, iterations = 200)$p < 0.05
    },
    logical(1)
  )
  expect_gte(mean(rejected), 0.02)
  expect_lte(mean(rejected), 0.10)
})

test_that("AC6: bootstrap p agrees with cor_to_p() at n = 2000", {
  skip_on_cran()
  set.seed(603)
  n <- 2000
  x <- stats::rnorm(n)
  e <- stats::residuals(stats::lm(stats::rnorm(n) ~ x))
  x <- as.numeric(scale(x))
  e <- as.numeric(scale(e))
  d <- data.frame(x = x, y = 0.04 * x + sqrt(1 - 0.04^2) * e)
  expect_equal(stats::cor(d$x, d$y), 0.04)
  out <- seeded(604, cor_test(d, "x", "y", bootstrap = TRUE, iterations = 2000))
  expect_lt(abs(out$p - cor_to_p(out$r, n = n)$p), 0.05)
})

test_that("AC6: law-school SE matches Efron & Tibshirani", {
  # transcription control: Efron & Tibshirani report r = 0.776
  expect_equal(
    stats::cor(law_school$LSAT, law_school$GPA),
    0.776,
    tolerance = 0.001
  )
  out <- seeded(
    605,
    cor_test(law_school, "LSAT", "GPA", bootstrap = TRUE, iterations = 2000)
  )
  expect_lt(abs(out$SE - 0.132), 0.01)
})

for (spec in method_sweep[vapply(
  method_sweep,
  function(s) s$method %in% odd_methods,
  logical(1)
)]) {
  test_that(paste("AC6: sign flip and affine map of y:", spec$label), {
    skip_sweep_entry(spec)
    d <- iris_integer()
    original <- seeded(
      606,
      cor_test(
        d,
        spec$x,
        spec$y,
        method = spec$method,
        bootstrap = TRUE,
        iterations = 50
      )
    )

    # wdm's Blomqvist coefficient counts ties at the median asymmetrically, so
    # the estimator itself is not odd under negation on resamples with ties
    if (spec$method != "blomqvist") {
      neg <- d
      neg[[spec$y]] <- -neg[[spec$y]]
      flipped <- seeded(
        606,
        cor_test(
          neg,
          spec$x,
          spec$y,
          method = spec$method,
          bootstrap = TRUE,
          iterations = 50
        )
      )
      expect_equal(flipped$CI_low, -original$CI_high, tolerance = 1e-8)
      expect_equal(flipped$CI_high, -original$CI_low, tolerance = 1e-8)
      expect_equal(flipped$SE, original$SE, tolerance = 1e-8)
      expect_identical(flipped$p, original$p)
    }

    aff <- d
    aff[[spec$y]] <- 2 * aff[[spec$y]] + 5
    mapped <- seeded(
      606,
      cor_test(
        aff,
        spec$x,
        spec$y,
        method = spec$method,
        bootstrap = TRUE,
        iterations = 50
      )
    )
    expect_equal(mapped$CI_low, original$CI_low, tolerance = 1e-8)
    expect_equal(mapped$CI_high, original$CI_high, tolerance = 1e-8)
    expect_equal(mapped$SE, original$SE, tolerance = 1e-8)
    expect_equal(mapped$p, original$p, tolerance = 1e-8)
  })
}

# Extra invariants beyond the criteria -----------------------------------------

test_that("coherence: p and the interval agree on excluding 0", {
  skip_on_cran()
  set.seed(701)
  for (i in seq_len(100)) {
    d <- sim_bvn(50, 0.2)
    out <- cor_test(d, "x", "y", bootstrap = TRUE, iterations = 200, ci = 0.95)
    B <- attr(out, "iterations")
    if (abs(out$p - 0.05) < 4 / (B + 1)) {
      next
    }
    expect_identical(out$p < 0.05, out$CI_low > 0 || out$CI_high < 0)
  }
})

test_that("negative control: y = x^2 + noise", {
  skip_on_cran()
  set.seed(702)
  res <- vapply(
    seq_len(200),
    function(i) {
      x <- stats::rnorm(50)
      d <- data.frame(x = x, y = x^2 + stats::rnorm(50))
      c(
        boot = cor_test(d, "x", "y", bootstrap = TRUE, iterations = 200)$p <
          0.05,
        fisher = cor_test(d, "x", "y", bootstrap = FALSE)$p < 0.05
      )
    },
    c(boot = NA, fisher = NA)
  )
  expect_gte(mean(res["boot", ]), 0.02)
  expect_lte(mean(res["boot", ]), 0.11)
  expect_gt(mean(res["fisher", ]), 0.20)
})
