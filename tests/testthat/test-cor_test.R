
test_that("cor_test frequentist", {
  expect_error(cor_test(iris, Petal.Length, Petal.Width))

  out <- cor_test(iris, "Petal.Length", "Petal.Width")
  expect_equal(out$r, 0.962, tolerance = 0.01)
})

test_that("cor_test kendall", {
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "kendall")
  out2 <- stats::cor.test(iris$Petal.Length, iris$Petal.Width, method = "kendall")

  expect_equal(out$tau, out2$estimate[[1]], tolerance = 0.001)
  expect_equal(out$p, out2$p.value[[1]], tolerance = 0.001)
})


test_that("cor_test bayesian", {
  if (require("BayesFactor", quietly = TRUE)) {
    out <- cor_test(iris, "Petal.Length", "Petal.Width", bayesian = TRUE)
    expect_equal(out$r, 0.9591191, tolerance = 0.01)

    set.seed(123)
    df_1 <- cor_test(iris, "Petal.Length", "Petal.Width", bayesian = TRUE)

    set.seed(123)
    df_2 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "auto", bayesian = TRUE)
    expect_equal(df_1, df_2, tolerance = 0.001)

    out2 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "spearman", bayesian = TRUE)
    expect_equal(out2$rho, 0.9323004, tolerance = 0.01)

    df <- iris
    df$Petal.Length2 <- df$Petal.Length
    out3 <- cor_test(df, "Petal.Length", "Petal.Length2", bayesian = TRUE)
    expect_equal(out3$rho, 1.000, tolerance = 0.01)

    out4 <- .cor_test_bayes_base(df$Petal.Length[1], df$Petal.Length2[1])
    expect_equal(out4$rho, 1.000, tolerance = 0.01)

    set.seed(123)
    out5 <- cor_test(mtcars, "wt", "mpg", method = "shepherd", bayesian = TRUE)
    expect_equal(out5$rho, -0.7795719, tolerance = 0.01)

    set.seed(123)
    out6 <- cor_test(mtcars, "wt", "mpg", method = "gaussian", bayesian = TRUE)
    expect_equal(out6$rho, -0.8294838, tolerance = 0.01)

    # unsupported
    expect_error(cor_test(mtcars, "wt", "mpg", method = "biserial", bayesian = TRUE))
    expect_error(cor_test(mtcars, "wt", "mpg", method = "polychoric", bayesian = TRUE))
    expect_error(cor_test(mtcars, "wt", "mpg", method = "tetrachoric", bayesian = TRUE))
    expect_error(cor_test(mtcars, "wt", "mpg", method = "biweight", bayesian = TRUE))
    expect_error(cor_test(mtcars, "wt", "mpg", method = "distance", bayesian = TRUE))
    expect_error(cor_test(mtcars, "wt", "mpg", method = "percentage", bayesian = TRUE))
    expect_error(cor_test(mtcars, "wt", "mpg", method = "blomqvist", bayesian = TRUE))
    expect_error(cor_test(mtcars, "wt", "mpg", method = "hoeffding", bayesian = TRUE))
    expect_error(cor_test(mtcars, "wt", "mpg", method = "gamma", bayesian = TRUE))
  }
})

test_that("cor_test tetrachoric", {
  if (require("psych", quietly = TRUE)) {
    data <- iris
    data$Sepal.Width_binary <- ifelse(data$Sepal.Width > 3, 1, 0)
    data$Petal.Width_binary <- ifelse(data$Petal.Width > 1.2, 1, 0)

    # With Factors / Binary
    out <- cor_test(data, "Sepal.Width_binary", "Petal.Width_binary", method = "tetrachoric")
    expect_equal(out$rho, -0.526, tolerance = 0.01)

    data$Petal.Width_ordinal <- as.factor(round(data$Petal.Width))
    data$Sepal.Length_ordinal <- as.factor(round(data$Sepal.Length))
    out <- cor_test(data, "Petal.Width_ordinal", "Sepal.Length_ordinal", method = "polychoric")
    # Curently CRAN checks show two possible results for this:
    if (isTRUE(all.equal(out$rho, 0.7507764, tolerance = 0.1))) {
      expect_equal(out$rho, 0.7507764, tolerance = 0.1)
    } else {
      expect_equal(out$rho, 0.528, tolerance = 0.01)
    }

    out <- cor_test(data, "Sepal.Width", "Sepal.Length_ordinal", method = "polychoric")
    expect_equal(out$rho, -0.144, tolerance = 0.01)

    # Biserial
    out <- cor_test(data, "Sepal.Width", "Petal.Width_binary", method = "pointbiserial")
    expect_equal(out$rho, -0.3212561, tolerance = 0.01)

    out <- cor_test(data, "Sepal.Width", "Petal.Width_binary", method = "biserial")
    expect_equal(out$rho, -0.403, tolerance = 0.01)
    out_psych <- psych::biserial(data[["Sepal.Width"]], data[["Petal.Width_binary"]])[1]
  }
})


test_that("cor_test robust", {
  out1 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "pearson", robust = TRUE)
  out2 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "spearman", robust = FALSE)
  expect_equal(out1$r, out2$rho, tolerance = 0.01)
})


test_that("cor_test distance", {
  if (require("energy", quietly = TRUE)) {
    out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "distance")
    comparison <- energy::dcorT.test(iris$Petal.Length, iris$Petal.Width)
    expect_equal(out$r, as.numeric(comparison$estimate), tolerance = 0.001)
    expect_identical(out$Method, "Distance (Bias Corrected)")

    # correction
    df1 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "distance", corrected = FALSE)
    df2 <- .cor_test_distance(iris, "Petal.Length", "Petal.Width", corrected = FALSE)

    expect_equal(df1$r, df2$r, tolerance = 0.001)
    expect_identical(df2$Method, "Distance")
  }
})


test_that("cor_test percentage", {
  if (require("WRS2", quietly = TRUE)) {
    out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "percentage")
    comparison <- WRS2::pbcor(iris$Petal.Length, iris$Petal.Width)
    expect_equal(out$r, as.numeric(comparison$cor), tolerance = 0.01)
  }
})


test_that("cor_test shepherd", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "shepherd")
  expect_equal(out$r, as.numeric(0.94762), tolerance = 0.01)

  if (require("BayesFactor", quietly = TRUE)) {
    set.seed(333)
    out2 <- cor_test(iris, "Petal.Length", "Petal.Width", method = "shepherd", bayesian = TRUE)
    expect_equal(out2$rho, as.numeric(0.9429992), tolerance = 0.01)
  }
})


test_that("cor_test blomqvist", {
  if (require("wdm", quietly = TRUE)) {
    set.seed(333)
    out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "blomqvist")
    expect_equal(out$r, as.numeric(0.9066667), tolerance = 0.01)
  }
})

test_that("cor_test hoeffding and somers", {
  if (require("Hmisc", quietly = TRUE)) {
    set.seed(333)
    out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "hoeffding")
    expect_equal(out$r, as.numeric(0.5629277), tolerance = 0.01)

    set.seed(333)
    df <- data.frame(x = 1:6, y = c(0, 0, 1, 0, 1, 1))
    out2 <- cor_test(df, "y", "x", method = "somers")
    expect_equal(out2$Dxy, as.numeric(0.7777778), tolerance = 0.01)
  }
})

test_that("cor_test gamma", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "gamma")
  expect_equal(out$r, as.numeric(0.8453925), tolerance = 0.01)
})

test_that("cor_test gaussian", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "gaussian")
  expect_equal(out$r, as.numeric(0.87137), tolerance = 0.01)

  if (requireNamespace("BayesFactor")) {
    out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "gaussian", bayesian = TRUE)
    expect_equal(out$r, as.numeric(0.8620878), tolerance = 0.01)
  }
})



# Additional arguments ----------------------------------------------------


test_that("cor_test one-sided p value", {
  baseline <- cor.test(iris$Petal.Length, iris$Petal.Width, alternative = "greater")

  out <- cor_test(iris, "Petal.Length", "Petal.Width", alternative = "greater")
  expect_equal(out$p, baseline$p.value, tolerance = 0.000001)
})



# Edge cases --------------------------------------------------------------
test_that("cor_test 2 valid observations", {
  out <- correlation(data.frame(v2 = c(2, 1, 1, 2), v3 = c(1, 2, NA, NA)))
  expect_true(is.na(out$r))
})
