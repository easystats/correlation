
test_that("cor_to_cov", {
  cor <- cor(iris[1:4])
  cov <- cov(iris[1:4])
  cov2 <- cor_to_cov(cor, var = sapply(iris[1:4], var))
  expect_equal(max(cov - cov2), 0, tolerance = 0.0001)
})

test_that("matrix_inverse works", {
  m <- matrix_inverse(cor(iris[1:4]))
  m2 <- solve(cor(iris[1:4]))
  expect_equal(max(m - m2), 0, tolerance = 0.0001)
})

test_that("is.cor works", {
  expect_true(is.cor(cor(mtcars)))
  expect_false(is.cor(as.matrix(anscombe)))
})

test_that("z_fisher works", {
  expect_equal(z_fisher(r = 0.7), 0.8673005, tolerance = 0.001)
  expect_equal(z_fisher(z = 0.867), 0.6998467, tolerance = 0.001)
})

test_that("simulate_simpson works", {
  skip_if_not_installed("MASS")

  set.seed(123)
  df <- simulate_simpson(n = 100, groups = 5, r = 0.5)
  expect_equal(dim(df), c(500L, 3L))
})
