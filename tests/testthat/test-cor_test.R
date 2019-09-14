context("cor_test")



test_that("cor_test frequentist", {
  data <- iris

  testthat::expect_error(cor_test(data, Petal.Length, Petal.Width))

  out <- cor_test(data, "Petal.Length", "Petal.Width")
  testthat::expect_equal(out$r, 0.962, tol = 0.01)

  out <- cor_test(data, "Petal.Length", "Petal.Width", bayesian=TRUE)
  testthat::expect_equal(out$r, 0.962, tol = 0.01)

  data$Sepal.Width_binary <- ifelse(data$Sepal.Width > 3, 1, 0)
  data$Petal.Width_binary <- ifelse(data$Petal.Width > 1.2, 1, 0)
  out <- cor_test(data, "Sepal.Width_binary", "Petal.Width_binary", method = "tetrachoric")
  testthat::expect_equal(out$rho, -0.526, tol = 0.01)
})
