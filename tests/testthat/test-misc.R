context("Miscellaenous")



test_that("cor2cov", {
  cor <- cor(iris[1:4])
  cov <- cov(iris[1:4])
  cov2 <- cor2cov(cor, var = sapply(iris[1:4], var))
  testthat::expect_equal(max(cov - cov2), 0, tol = 0.0001)
})



test_that("matrix_inverse", {
  m <- matrix_inverse(cor(iris[1:4]))
  m2 <- solve(cor(iris[1:4]))
  testthat::expect_equal(max(m - m2), 0, tol = 0.0001)
})
