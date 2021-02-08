test_that("cor_to_cov", {
  cor <- cor(iris[1:4])
  expect_error(cor_to_cov(cor))

  expect_error(cor_to_cov(as.matrix(rnorm(5))))

  expect_error(cor_to_cov(cor, sd = sapply(iris[1:3], sd)))
})
