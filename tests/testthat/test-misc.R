
test_that("Mahalanobis", {
  d <- distance_mahalanobis(data = iris[, 1:4], robust = FALSE)
  expect_equal(ncol(d), 1)
  d <- distance_mahalanobis(data = iris[, 1:4], robust = TRUE)
  expect_equal(ncol(d), 3)
})



test_that("cor_to_cov", {
  cor <- cor(iris[1:4])
  cov <- cov(iris[1:4])
  cov2 <- cor_to_cov(cor, var = sapply(iris[1:4], var))
  expect_equal(max(cov - cov2), 0, tolerance = 0.0001)
})



test_that("matrix_inverse", {
  m <- matrix_inverse(cor(iris[1:4]))
  m2 <- solve(cor(iris[1:4]))
  expect_equal(max(m - m2), 0, tolerance = 0.0001)
})
