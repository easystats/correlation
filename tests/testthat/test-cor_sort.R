test_that("cor_sort", {
  # Basic -------------------------------------------------------------------

  # Square
  r1 <- cor(mtcars)
  expect_equal(as.numeric(diag(r1)), rep(1, ncol(mtcars)))
  # heatmap(r1, Rowv = NA, Colv = NA)  # visualize

  r1sort <- cor_sort(r1)
  expect_equal(as.numeric(diag(r1sort)), rep(1, ncol(mtcars)))
  # heatmap(r1sort, Rowv = NA, Colv = NA)  # visualize

  # Non-square
  r2 <- cor(mtcars[names(mtcars)[1:5]], mtcars[names(mtcars)[6:11]])
  expect_equal(rownames(r2), names(mtcars)[1:5])
  expect_identical(colnames(r2), c("wt", "qsec", "vs", "am", "gear", "carb"))
  expect_identical(rownames(r2), c("mpg", "cyl", "disp", "hp", "drat"))
  # heatmap(r2, Rowv = NA, Colv = NA)  # visualize

  r2sort <- cor_sort(r2)
  expect_false(all(rownames(r2sort) == names(mtcars)[1:5]))
  expect_identical(
    colnames(r2sort),
    c("am", "gear", "qsec", "vs", "wt", "carb")
  )
  expect_identical(rownames(r2sort), c("drat", "disp", "hp", "cyl", "mpg"))
  # heatmap(r2sort, Rowv = NA, Colv = NA)  # visualize

  # correlation() -----------------------------------------------------------
  # Square
  rez1 <- correlation::correlation(mtcars)
  rez1sort <- cor_sort(rez1)
  expect_false(all(rez1$Parameter1 == rez1sort$Parameter1))

  # Non-square
  rez2 <- correlation::correlation(
    mtcars[names(mtcars)[1:5]],
    mtcars[names(mtcars)[6:11]]
  )
  rez2sort <- cor_sort(rez2)
  expect_false(all(rez2$Parameter1 == rez2sort$Parameter1))

  # summary(correlation()) --------------------------------------------------
  # Square
  rez1sum <- summary(rez1) # TODO: doesn't work with non-redundant
  # TODO: fix
  expect_error(cor_sort(rez1sum))

  rez1sum <- summary(rez1, redundant = TRUE)
  rez1sumsort <- cor_sort(rez1sum)
  expect_false(all(rownames(rez1sumsort) == rownames(rez1sum)))

  # Non-square
  rez2sum <- summary(rez2)
  rez2sumsort <- cor_sort(rez2sum)
  expect_false(all(rownames(rez2sumsort) == rownames(rez2sum)))

  # as.matrix(correlation()) ------------------------------------------------
  # TODO.
  m1 <- as.matrix(rez1)
  # m1sort <- as.matrix(rez1sort)
})


# Undefined correlations --------------------------------------------------

# Variables that were never observed together yield NA correlations, which
# cannot be handed to hclust() as-is (#366)
make_disjoint_data <- function(n = 100) {
  set.seed(1)
  data.frame(
    a = rnorm(n),
    b = c(rnorm(n / 2), rep(NA, n / 2)),
    c = c(rep(NA, n / 2), rnorm(n / 2))
  )
}

test_that("cor_sort works with undefined correlations", {
  d <- make_disjoint_data()
  rez <- correlation(d, redundant = TRUE)
  m <- summary(rez, redundant = TRUE)

  # The pair is indeed undefined
  expect_true(is.na(rez$r[rez$Parameter1 == "b" & rez$Parameter2 == "c"]))
  expect_true(attributes(m)$redundant)

  # easycorrelation (long form)
  sorted <- cor_sort(rez)
  expect_s3_class(sorted, "easycorrelation")
  expect_setequal(as.character(sorted$Parameter1), c("a", "b", "c"))
  # NAs are preserved, never imputed away
  expect_true(is.na(sorted$r[
    sorted$Parameter1 == "b" & sorted$Parameter2 == "c"
  ]))

  # easycormatrix (summary)
  sorted_matrix <- cor_sort(m)
  expect_identical(sorted_matrix$Parameter, c("c", "a", "b"))
  expect_true(is.na(sorted_matrix$c[sorted_matrix$Parameter == "b"]))

  # plain matrix
  sorted_plain <- cor_sort(as.matrix(rez))
  expect_identical(rownames(sorted_plain), c("c", "a", "b"))
  expect_identical(colnames(sorted_plain), c("c", "a", "b"))
  expect_true(is.na(sorted_plain["b", "c"]))
})

test_that("cor_sort na_action options", {
  d <- make_disjoint_data()
  m <- summary(correlation(d, redundant = TRUE), redundant = TRUE)

  # "zero" reproduces replacing the NAs by 0 by hand
  manual <- m
  manual[-1] <- lapply(manual[-1], function(x) ifelse(is.na(x), 0, x))
  expect_identical(
    cor_sort(m, na_action = "zero")$Parameter,
    cor_sort(manual)$Parameter
  )

  # here "infer" has no information to work with, so it agrees with "zero"
  expect_identical(
    cor_sort(m, na_action = "infer")$Parameter,
    cor_sort(m, na_action = "zero")$Parameter
  )

  # "omit" keeps every variable, with the undefined ones appended at the end
  omitted <- cor_sort(m, na_action = "omit")
  expect_setequal(omitted$Parameter, c("a", "b", "c"))
  expect_identical(omitted$Parameter[1], "a")
  expect_setequal(omitted$Parameter[2:3], c("b", "c"))

  # "error" is informative about which pairs are undefined
  expect_error(cor_sort(m, na_action = "error"), regexp = "b - c")

  # unknown values are rejected
  expect_error(cor_sort(m, na_action = "sausage"))
})

test_that("cor_sort infers undefined correlations from the other variables", {
  # b and c belong to the same block but were never observed together
  set.seed(7)
  n <- 400
  f <- rnorm(n)
  mk <- function(load) load * f + sqrt(1 - load^2) * rnorm(n)
  d <- data.frame(m1 = mk(0.97), m2 = mk(0.97), b = mk(0.97), c = mk(0.97))
  truth <- cor(d)["b", "c"]

  d$b[1:(n / 2)] <- NA
  d$c[(n / 2 + 1):n] <- NA
  m <- as.matrix(correlation(d, redundant = TRUE))
  m <- matrix(as.numeric(m), nrow(m), dimnames = dimnames(m))
  expect_true(is.na(m["b", "c"]))

  # The inferred value is close to the truth, unlike 0
  inferred <- correlation:::.complete_cormatrix(m, na_action = "infer")
  expect_equal(inferred["b", "c"], truth, tolerance = 0.1)
  expect_true(abs(inferred["b", "c"] - truth) < abs(0 - truth))

  # ... and it is symmetric, and leaves defined correlations untouched
  expect_identical(inferred["b", "c"], inferred["c", "b"])
  expect_identical(inferred["m1", "m2"], m["m1", "m2"])

  # Zero imputation splits the block, inference keeps it together
  block <- c("m1", "m2", "b", "c")
  expect_setequal(rownames(cor_sort(m, na_action = "infer")), block)
})

test_that("cor_sort completion converges with several undefined pairs", {
  set.seed(11)
  n <- 400
  f <- rnorm(n)
  mk <- function(load) load * f + sqrt(1 - load^2) * rnorm(n)
  d <- data.frame(m1 = mk(0.9), b = mk(0.9), c = mk(0.9), e = mk(0.9))
  truth <- cor(d)

  # b, c and e are pairwise never observed together
  d$b[1:266] <- NA
  d$c[c(1:133, 267:400)] <- NA
  d$e[134:400] <- NA
  m <- as.matrix(correlation(d, redundant = TRUE))
  m <- matrix(as.numeric(m), nrow(m), dimnames = dimnames(m))
  expect_identical(sum(is.na(m) & upper.tri(m)), 3L)

  inferred <- correlation:::.complete_cormatrix(m, na_action = "infer")
  expect_false(anyNA(inferred))
  expect_true(all(abs(inferred) <= 1))
  # the completion is a valid (positive semi-definite) correlation matrix
  eigenvalues <- eigen(inferred, symmetric = TRUE, only.values = TRUE)$values
  expect_gte(min(eigenvalues), -1e-8)
  # and beats zero for every undefined pair
  for (pair in list(c("m1", "b"), c("b", "c"), c("b", "e"), c("c", "e"))) {
    if (is.na(m[pair[1], pair[2]])) {
      expect_lt(
        abs(inferred[pair[1], pair[2]] - truth[pair[1], pair[2]]),
        abs(truth[pair[1], pair[2]])
      )
    }
  }

  expect_setequal(rownames(cor_sort(m)), c("m1", "b", "c", "e"))
})

test_that("cor_sort completion handles degenerate cases", {
  # No mediating variable at all
  m <- matrix(
    c(1, NA, NA, 1),
    nrow = 2,
    dimnames = list(c("b", "c"), c("b", "c"))
  )
  expect_identical(correlation:::.complete_cormatrix(m)[["b", "c"]], 0)

  # Perfectly collinear mediators make the submatrix singular
  set.seed(3)
  n <- 300
  f <- rnorm(n)
  d <- data.frame(
    m1 = f,
    m2 = f,
    b = f + rnorm(n, sd = 0.3),
    c = f + rnorm(n, sd = 0.3)
  )
  d$b[1:(n / 2)] <- NA
  d$c[(n / 2 + 1):n] <- NA
  m <- as.matrix(correlation(d, redundant = TRUE))
  m <- matrix(as.numeric(m), nrow(m), dimnames = dimnames(m))
  inferred <- correlation:::.complete_cormatrix(m)
  expect_false(anyNA(inferred))
  expect_true(all(abs(inferred) <= 1))

  # Everything undefined: "omit" cannot sort, and says so
  m2 <- matrix(
    c(1, NA, NA, 1),
    nrow = 2,
    dimnames = list(c("b", "c"), c("b", "c"))
  )
  expect_warning(
    expect_identical(rownames(cor_sort(m2, na_action = "omit")), c("b", "c")),
    regexp = "Fewer than two"
  )
})

test_that("cor_sort works on non-square matrices with undefined correlations", {
  d <- mtcars
  d$mpg[1:20] <- NA
  d$carb[21:32] <- NA
  m <- cor(d[1:5], d[6:11], use = "pairwise.complete.obs")
  expect_true(anyNA(m))

  sorted <- cor_sort(m)
  expect_setequal(rownames(sorted), rownames(m))
  expect_setequal(colnames(sorted), colnames(m))
  expect_error(cor_sort(m, na_action = "error"), regexp = "undefined")
})

test_that("cor_sort redundancy check reads the attribute, not the NAs", {
  d <- make_disjoint_data()
  rez <- correlation(d, redundant = TRUE)

  # Redundant but with NAs: supported
  expect_no_error(cor_sort(summary(rez, redundant = TRUE)))

  # Genuinely non-redundant: still refused
  expect_error(
    cor_sort(summary(rez)),
    regexp = "Non-redundant"
  )

  # Fallback when the attribute is absent: masked triangle is detected, while
  # symmetric NAs are not
  triangular <- matrix(
    c(1, NA, NA, 0.5, 1, NA, 0.2, 0.3, 1),
    nrow = 3,
    dimnames = list(letters[1:3], letters[1:3])
  )
  expect_true(correlation:::.is_nonredundant_matrix(list(), triangular))

  undefined <- matrix(
    c(1, 0.5, 0.2, 0.5, 1, NA, 0.2, NA, 1),
    nrow = 3,
    dimnames = list(letters[1:3], letters[1:3])
  )
  expect_false(correlation:::.is_nonredundant_matrix(list(), undefined))
})
