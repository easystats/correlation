
test_that("cor_test frequentist", {
  testthat::expect_error(cor_test(iris, Petal.Length, Petal.Width))

  out <- cor_test(iris, "Petal.Length", "Petal.Width")
  testthat::expect_equal(out$r, 0.962, tolerance = 0.01)
})


test_that("cor_test bayesian", {
  if (requireNamespace("BayesFactor")) {
    out <- cor_test(iris, "Petal.Length", "Petal.Width", bayesian = TRUE)
    testthat::expect_equal(out$r, 0.962, tolerance = 0.01)
  }
})

test_that("cor_test tetrachoric", {
  if (requireNamespace("psych")) {
    data <- iris
    data$Sepal.Width_binary <- ifelse(data$Sepal.Width > 3, 1, 0)
    data$Petal.Width_binary <- ifelse(data$Petal.Width > 1.2, 1, 0)

    # With Factors / Binary
    out <- cor_test(data, "Sepal.Width_binary", "Petal.Width_binary", method = "tetrachoric")
    testthat::expect_equal(out$rho, -0.526, tolerance = 0.01)

    data$Petal.Width_ordinal <- as.factor(round(data$Petal.Width))
    data$Sepal.Length_ordinal <- as.factor(round(data$Sepal.Length))
    out <- cor_test(data, "Petal.Width_ordinal", "Sepal.Length_ordinal", method = "polychoric")
    # Curently CRAN checks show two possible results for this:
    if (isTRUE(all.equal(out$rho, 0.7507764, tolerance = 0.1))) {
      testthat::expect_equal(out$rho, 0.7507764, tolerance = 0.1)
    } else {
      testthat::expect_equal(out$rho, 0.528, tolerance = 0.01)
    }

    out <- cor_test(data, "Sepal.Width", "Sepal.Length_ordinal", method = "polychoric")
    testthat::expect_equal(out$rho, -0.144, tolerance = 0.01)

    # Biserial
    out <- cor_test(data, "Sepal.Width", "Petal.Width_binary", method = "pointbiserial")
    testthat::expect_equal(out$rho, -0.3212561, tolerance = 0.01)

    out <- cor_test(data, "Sepal.Width", "Petal.Width_binary", method = "biserial")
    testthat::expect_equal(out$rho, -0.403, tolerance = 0.01)
    out_psych <- psych::biserial(data[["Sepal.Width"]], data[["Petal.Width_binary"]])[1]
  }
})


test_that("cor_test robust", {
  out1 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "pearson", robust = TRUE)
  out2 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "spearman", robust = FALSE)
  testthat::expect_equal(out1$r, out2$rho, tolerance = 0.01)
})


test_that("cor_test distance", {
  if (requireNamespace("energy")) {
    out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "distance")
    comparison <- energy::dcor.ttest(iris$Petal.Length, iris$Petal.Width)
    testthat::expect_equal(out$r, as.numeric(comparison$estimate), tolerance = 0.01)
  }
})


test_that("cor_test percentage", {
  if (requireNamespace("WRS2")) {
    out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "percentage")
    comparison <- WRS2::pbcor(iris$Petal.Length, iris$Petal.Width)
    testthat::expect_equal(out$r, as.numeric(comparison$cor), tolerance = 0.01)
  }
})


test_that("cor_test shepherd", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "shepherd")
  testthat::expect_equal(out$r, as.numeric(0.94762), tolerance = 0.01)
})


test_that("cor_test blomqvist", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "blomqvist")
  testthat::expect_equal(out$r, as.numeric(0.9066667), tolerance = 0.01)
})

test_that("cor_test hoeffding", {
  if (requireNamespace("Hmisc")) {
    set.seed(333)
    out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "hoeffding")
    testthat::expect_equal(out$r, as.numeric(0.5629277), tolerance = 0.01)
  }
})

test_that("cor_test gamma", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "gamma")
  testthat::expect_equal(out$r, as.numeric(0.8453925), tolerance = 0.01)
})

test_that("cor_test gaussian", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "gaussian")
  testthat::expect_equal(out$r, as.numeric(0.87137), tolerance = 0.01)

  if (requireNamespace("BayesFactor")) {
    out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "gaussian", bayesian = TRUE)
    testthat::expect_equal(out$r, as.numeric(0.8620878), tolerance = 0.01)
  }
})



# Edge cases --------------------------------------------------------------
test_that("cor_test 2 valid observations", {
  out <- correlation(data.frame(v2 = c(2, 1, 1, 2), v3 = c(1, 2, NA, NA)))
  testthat::expect_true(is.na(out$r))
})
