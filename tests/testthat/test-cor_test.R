context("cor_test")



test_that("cor_test frequentist", {
  testthat::expect_error(cor_test(iris, Petal.Length, Petal.Width))

  out <- cor_test(iris, "Petal.Length", "Petal.Width")
  testthat::expect_equal(out$r, 0.962, tol = 0.01)

  out <- cor_test(iris, "Petal.Length", "Petal.Width", bayesian=TRUE)
  testthat::expect_equal(out$r, 0.962, tol = 0.01)
})
