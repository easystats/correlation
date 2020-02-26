context("cor_test")



test_that("cor_test frequentist", {
  data <- iris

  testthat::expect_error(cor_test(data, Petal.Length, Petal.Width))

  out <- cor_test(data, "Petal.Length", "Petal.Width")
  testthat::expect_equal(out$r, 0.962, tol = 0.01)

  out <- cor_test(data, "Petal.Length", "Petal.Width", bayesian = TRUE)
  testthat::expect_equal(out$r, 0.962, tol = 0.01)

  data$Sepal.Width_binary <- ifelse(data$Sepal.Width > 3, 1, 0)
  data$Petal.Width_binary <- ifelse(data$Petal.Width > 1.2, 1, 0)
  out <- cor_test(data, "Sepal.Width_binary", "Petal.Width_binary", method = "tetrachoric")
  testthat::expect_equal(out$rho, -0.526, tol = 0.01)

  out <- cor_test(data, "Sepal.Width", "Petal.Width_binary", method = "tetrachoric")
  testthat::expect_equal(out$rho, -0.403, tol = 0.01)

  data$Petal.Width_ordinal <- as.factor(round(data$Petal.Width))
  data$Sepal.Length_ordinal <- as.factor(round(data$Sepal.Length))
  out <- cor_test(data, "Petal.Width_ordinal", "Sepal.Length_ordinal", method = "polychoric")
  testthat::expect_equal(out$rho, 0.751, tol = 0.05)

  out <- cor_test(data, "Sepal.Width", "Sepal.Length_ordinal", method = "polychoric")
  testthat::expect_equal(out$rho, -0.144, tol = 0.01)
})


test_that("cor_test robust", {
  out1 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "pearson", robust = TRUE)
  out2 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "spearman", robust = FALSE)
  testthat::expect_equal(out1$r, out2$rho, tol = 0.01)
})


test_that("cor_test distance", {
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "distance")
  comparison <- energy::dcor.ttest(iris$Petal.Length, iris$Petal.Width)
  testthat::expect_equal(out$r, as.numeric(comparison$estimate), tol = 0.01)
})


test_that("cor_test percentage", {
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "percentage")
  comparison <- WRS2::pbcor(iris$Petal.Length, iris$Petal.Width)
  testthat::expect_equal(out$r, as.numeric(comparison$cor), tol = 0.01)
})


test_that("cor_test shepherd", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "shepherd")
  testthat::expect_equal(out$r, as.numeric(0.94762), tol = 0.01)
})
