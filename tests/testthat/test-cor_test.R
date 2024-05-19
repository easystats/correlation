test_that("cor_test names (x,y)", {
  expect_error(cor_test(Petal.Length, Petal.Width, data = iris))

  out <- cor_test("Petal.Length", "Petal.Width", data = iris)
  out2 <- parameters::model_parameters(stats::cor.test(iris$Petal.Length, iris$Petal.Width))

  expect_equal(out$r, out2$r, tolerance = 0.01)
  expect_equal(out$CI_low, out2$CI_low, tolerance = 0.01)
  expect_equal(out$CI_high, out2$CI_high, tolerance = 0.01)
})

test_that("cor_test inputs are columns from data.frame and/or vector", {
  x <- iris$Petal.Length
  y <- iris$Petal.Width

  expect_error(cor_test("Petal.Length", "Petal.Width"))
  expect_error(cor_test(x, "Petal.Width"))

  out <- cor_test("Petal.Length", "Petal.Width", data = iris)
  out2 <- cor_test(x, y)

  expect_equal(out[-(1:2)], out2[-(1:2)])
})

test_that("cor_test kendall tau-a", {
  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "kendall", tau_type = "a")
  completeCase <- stats::complete.cases(iris[["Petal.Length"]], iris[["Petal.Width"]])
  var_x <- iris[["Petal.Length"]][completeCase]
  var_y <- iris[["Petal.Width"]][completeCase]
  tab <- table(var_x, var_y)
  n <- sum(tab)
  ConDisParams <- DescTools::ConDisPairs(tab)[3:4]
  z <- (ConDisParams$C - ConDisParams$D) / sqrt(n * (n - 1) * (2 * n + 5) / 18)

  expect_equal(out$tau, (ConDisParams$C - ConDisParams$D)/(n * (n - 1)/2), tolerance = 0.001)
  expect_equal(out$p, 2 * stats::pnorm(abs(z), lower.tail = FALSE), tolerance = 0.001)
})

test_that("cor_test kendall tau-b", {
  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "kendall")
  out2 <- stats::cor.test(iris$Petal.Length, iris$Petal.Width, method = "kendall")

  expect_equal(out$tau, out2$estimate[[1]], tolerance = 0.001)
  expect_equal(out$p, out2$p.value[[1]], tolerance = 0.001)
})

test_that("cor_test kendall tau-c", {
  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "kendall", tau_type = "c")
  out2 <- stats::cor.test(iris$Petal.Length, iris$Petal.Width, method = "kendall")

  completeCase <- stats::complete.cases(iris[["Petal.Length"]], iris[["Petal.Width"]])
  var_x <- iris[["Petal.Length"]][completeCase]
  var_y <- iris[["Petal.Width"]][completeCase]
  tab <- table(var_x, var_y)
  ConDisParams <- DescTools::ConDisPairs(tab)[3:4]

  expect_equal(out$tau, (ConDisParams$C - ConDisParams$D) * 2 * min(dim(tab))/(sum(tab)^2 * (min(dim(tab)) - 1)), tolerance = 0.001)
  expect_equal(out$p, out2$p.value[[1]], tolerance = 0.001)
})

test_that("cor_test bayesian", {
  skip_if_not_or_load_if_installed("BayesFactor")
  out <- cor_test("Petal.Length", "Petal.Width", data = iris, bayesian = TRUE)
  expect_equal(out$r, 0.9591191, tolerance = 0.01)

  out2 <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "spearman", bayesian = TRUE)
  expect_equal(out2$r, 0.9323004, tolerance = 0.01)

  df <- iris
  df$Petal.Length2 <- df$Petal.Length
  out3 <- cor_test("Petal.Length", "Petal.Length2", data = df, bayesian = TRUE)
  expect_equal(out3$r, 1.000, tolerance = 0.01)

  if (getRversion() >= "3.6") {
    set.seed(123)
    out4 <- cor_test("wt", "mpg", data = mtcars, method = "shepherd", bayesian = TRUE)
    expect_equal(out4$r, -0.7795719, tolerance = 0.01)

    set.seed(123)
    out5 <- cor_test("wt", "mpg", data = mtcars, method = "gaussian", bayesian = TRUE)
    expect_equal(out5$r, -0.8294838, tolerance = 0.01)
  }

  # unsupported
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "kendall", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "biserial", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "point-biserial", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "rank-biserial", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "polychoric", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "tetrachoric", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "biweight", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "distance", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "percentage", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "blomqvist", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "hoeffding", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "somers", bayesian = TRUE))
  expect_error(cor_test("wt", "mpg", data = mtcars, method = "gamma", bayesian = TRUE))
})

test_that("cor_test tetrachoric", {
  skip_if_not_or_load_if_installed("psych")
  skip_if_not_or_load_if_installed("polycor")

  data <- iris
  data$Sepal.Width_binary <- ifelse(data$Sepal.Width > 3, 1, 0)
  data$Petal.Width_binary <- ifelse(data$Petal.Width > 1.2, 1, 0)

  # With Factors / Binary
  out <- cor_test("Sepal.Width_binary", "Petal.Width_binary", data = data, method = "tetrachoric")
  expect_equal(out$r, -0.526, tolerance = 0.01)

  data$Petal.Width_ordinal <- as.factor(round(data$Petal.Width))
  data$Sepal.Length_ordinal <- as.factor(round(data$Sepal.Length))
  out <- cor_test("Petal.Width_ordinal", "Sepal.Length_ordinal", data = data, method = "polychoric")

  # Curently CRAN checks show two possible results for this:
  if (isTRUE(all.equal(out$r, 0.7507764, tolerance = 0.1))) {
    expect_equal(out$r, 0.7507764, tolerance = 0.1)
  } else {
    expect_equal(out$r, 0.528, tolerance = 0.01)
  }

  out <- cor_test("Sepal.Width", "Sepal.Length_ordinal", data = data, method = "polychoric")
  expect_equal(out$r, -0.144, tolerance = 0.01)

  # Biserial
  expect_error(cor_test("Sepal.Width", "Petal.Width", data = data, method = "biserial"))

  out <- cor_test("Sepal.Width", "Petal.Width_binary", data = data, method = "pointbiserial")
  expect_equal(out$r, -0.3212561, tolerance = 0.0001)

  out <- cor_test("Sepal.Width", "Petal.Width_binary", data = data, method = "biserial")
  out_psych <- psych::biserial(data[["Sepal.Width"]], data[["Petal.Width_binary"]])[1]
  expect_equal(out$r, out_psych, tolerance = 0.0001)

  out <- cor_test("Sepal.Width", "Petal.Width_binary", data = data, method = "rankbiserial")
  expect_equal(out$r, -0.003755053, tolerance = 0.0001)
})

test_that("cor_test robust", {
  out1 <- cor_test(datawizard::ranktransform(iris$Petal.Length, sign = FALSE, method = "average"), datawizard::ranktransform(iris$Petal.Width, sign = FALSE, method = "average"))
  out2 <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "spearman")
  expect_equal(out1$r, out2$r, tolerance = 0.01)
})

test_that("cor_test distance", {
  skip_if(getRversion() < "4.0")
  skip_if_not_or_load_if_installed("energy")

  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "distance")
  comparison <- energy::dcorT.test(iris$Petal.Length, iris$Petal.Width)
  expect_equal(out$r, as.numeric(comparison$estimate), tolerance = 0.001)
  expect_identical(out$Method, "Distance (Bias Corrected)")
})

test_that("cor_test percentage", {
  skip_if_not_or_load_if_installed("WRS2")

  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "percentage")
  comparison <- WRS2::pbcor(iris$Petal.Length, iris$Petal.Width)
  expect_equal(out$r, as.numeric(comparison$cor), tolerance = 0.01)
})

test_that("cor_test shepherd", {
  set.seed(333)
  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "shepherd")
  expect_equal(out$r, 0.94762, tolerance = 0.01)

  skip_if_not_or_load_if_installed("BayesFactor")
  set.seed(333)
  out2 <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "shepherd", bayesian = TRUE)
  expect_equal(out2$rho, 0.9429992, tolerance = 0.01)
})

test_that("cor_test blomqvist", {
  skip_if_not_or_load_if_installed("wdm")

  set.seed(333)
  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "blomqvist")
  expect_equal(out$r, 0.9066667, tolerance = 0.01)
})

test_that("cor_test hoeffding and somers", {
  skip_if_not_or_load_if_installed("Hmisc")
  set.seed(333)
  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "hoeffding")
  expect_equal(out$r, 0.5629277, tolerance = 0.01)

  set.seed(333)
  df <- data.frame(x = 1:6, y = c(0, 0, 1, 0, 1, 1))
  out2 <- cor_test("x", "y", data = df, method = "somers")
  expect_equal(out2$Dxy, 0.7777778, tolerance = 0.01)
})

test_that("cor_test gamma", {
  set.seed(333)
  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "gamma")
  expect_equal(out$r, 0.8453925, tolerance = 0.01)
})

test_that("cor_test gaussian", {
  skip_if_not_or_load_if_installed("BayesFactor")

  set.seed(333)
  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "gaussian")
  expect_equal(out$r, 0.87137, tolerance = 0.01)

  out <- cor_test("Petal.Length", "Petal.Width", data = iris, method = "gaussian", bayesian = TRUE)
  expect_equal(out$r, 0.8620878, tolerance = 0.01)
})

# Additional arguments ----------------------------------------------------

test_that("cor_test one-sided p value", {
  baseline <- cor.test(iris$Petal.Length, iris$Petal.Width, alternative = "greater")

  out <- cor_test("Petal.Length", "Petal.Width", data = iris, alternative = "greater")
  expect_equal(out$p, baseline$p.value, tolerance = 0.000001)
})

