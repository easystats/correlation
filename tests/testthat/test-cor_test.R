test_that("cor_test frequentist", {
  expect_error(cor_test(iris, Petal.Length, Petal.Width))

  out <- cor_test(iris, "Petal.Length", "Petal.Width")
  expect_equal(out$r, 0.962, tolerance = 0.01)
})

test_that("cor_test kendall", {
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "kendall")
  out2 <- stats::cor.test(
    iris$Petal.Length,
    iris$Petal.Width,
    method = "kendall"
  )

  expect_equal(out$tau, out2$estimate[[1]], tolerance = 0.001)
  expect_equal(out$p, out2$p.value[[1]], tolerance = 0.001)
})


test_that("cor_test bayesian", {
  skip_if_not_or_load_if_installed("BayesFactor")
  out <- cor_test(iris, "Petal.Length", "Petal.Width", bayesian = TRUE)
  expect_equal(out$rho, 0.9591191, tolerance = 0.01)

  set.seed(123)
  df_1 <- cor_test(iris, "Petal.Length", "Petal.Width", bayesian = TRUE)

  set.seed(123)
  df_2 <- cor_test(
    iris,
    "Petal.Length",
    "Petal.Width",
    method = "auto",
    bayesian = TRUE
  )
  expect_equal(df_1, df_2, tolerance = 0.001)

  out2 <- cor_test(
    iris,
    "Petal.Length",
    "Petal.Width",
    method = "spearman",
    bayesian = TRUE
  )
  expect_equal(out2$rho, 0.9323004, tolerance = 0.01)

  df <- iris
  df$Petal.Length2 <- df$Petal.Length
  out3 <- suppressWarnings(cor_test(
    df,
    "Petal.Length",
    "Petal.Length2",
    bayesian = TRUE
  ))
  expect_equal(out3$rho, 1.000, tolerance = 0.01)

  if (getRversion() >= "3.6") {
    set.seed(123)
    out5 <- cor_test(mtcars, "wt", "mpg", method = "shepherd", bayesian = TRUE)
    expect_equal(out5$rho, -0.7795719, tolerance = 0.01)

    set.seed(123)
    out6 <- cor_test(mtcars, "wt", "mpg", method = "gaussian", bayesian = TRUE)
    expect_equal(out6$rho, -0.8294838, tolerance = 0.01)
  }

  # unsupported
  expect_error(cor_test(
    mtcars,
    "wt",
    "mpg",
    method = "biserial",
    bayesian = TRUE
  ))
  expect_error(cor_test(
    mtcars,
    "wt",
    "mpg",
    method = "polychoric",
    bayesian = TRUE
  ))
  expect_error(cor_test(
    mtcars,
    "wt",
    "mpg",
    method = "tetrachoric",
    bayesian = TRUE
  ))
  expect_error(cor_test(
    mtcars,
    "wt",
    "mpg",
    method = "biweight",
    bayesian = TRUE
  ))
  expect_error(cor_test(
    mtcars,
    "wt",
    "mpg",
    method = "distance",
    bayesian = TRUE
  ))
  expect_error(cor_test(
    mtcars,
    "wt",
    "mpg",
    method = "percentage",
    bayesian = TRUE
  ))
  expect_error(cor_test(
    mtcars,
    "wt",
    "mpg",
    method = "blomqvist",
    bayesian = TRUE
  ))
  expect_error(cor_test(
    mtcars,
    "wt",
    "mpg",
    method = "hoeffding",
    bayesian = TRUE
  ))
  expect_error(cor_test(mtcars, "wt", "mpg", method = "gamma", bayesian = TRUE))
})

test_that("cor_test tetrachoric", {
  skip_if_not_or_load_if_installed("psych")
  skip_if_not_or_load_if_installed("polycor")
  data <- iris
  data$Sepal.Width_binary <- as.numeric(data$Sepal.Width > 3)
  data$Petal.Width_binary <- as.numeric(data$Petal.Width > 1.2)

  # With Factors / Binary
  out <- cor_test(
    data,
    "Sepal.Width_binary",
    "Petal.Width_binary",
    method = "tetrachoric"
  )
  expect_equal(out$rho, -0.526, tolerance = 0.01)

  data$Petal.Width_ordinal <- as.factor(round(data$Petal.Width))
  data$Sepal.Length_ordinal <- as.factor(round(data$Sepal.Length))
  out <- cor_test(
    data,
    "Petal.Width_ordinal",
    "Sepal.Length_ordinal",
    method = "polychoric"
  )

  # Curently CRAN checks show two possible results for this:
  if (isTRUE(all.equal(out$rho, 0.7507764, tolerance = 0.1))) {
    expect_equal(out$rho, 0.7507764, tolerance = 0.1)
  } else {
    expect_equal(out$rho, 0.528, tolerance = 0.01)
  }

  out <- cor_test(
    data,
    "Sepal.Width",
    "Sepal.Length_ordinal",
    method = "polychoric"
  )
  expect_equal(out$rho, -0.144, tolerance = 0.01)

  # Biserial
  out <- cor_test(
    data,
    "Sepal.Width",
    "Petal.Width_binary",
    method = "pointbiserial"
  )
  expect_equal(out$rho, -0.3212561, tolerance = 0.01)

  out <- cor_test(
    data,
    "Sepal.Width",
    "Petal.Width_binary",
    method = "biserial"
  )
  expect_equal(out$rho, -0.403, tolerance = 0.01)
  out_psych <- psych::biserial(
    data[["Sepal.Width"]],
    data[["Petal.Width_binary"]]
  )[1]

  set.seed(123)
  n <- 100
  k <- 5
  d <- data.frame(
    x1 = sample.int(k, n, TRUE),
    x2 = sample.int(k, n, TRUE),
    x3 = sample.int(k, n, TRUE),
    x4 = sample.int(k, n, TRUE)
  )
  expect_true(all(vapply(d, is.numeric, logical(1))))
  out <- correlation(d, method = "polychoric")
  expect_equal(
    out$rho,
    c(0.07729, -0.02453, -0.13999, 0.06508, -0.17158, 0.17863),
    tolerance = 1e-3
  )
})


test_that("cor_test robust", {
  out1 <- cor_test(
    iris,
    "Petal.Length",
    "Petal.Width",
    method = "pearson",
    ranktransform = TRUE
  )
  out2 <- cor_test(
    iris,
    "Petal.Length",
    "Petal.Width",
    method = "spearman",
    ranktransform = FALSE
  )
  expect_equal(out1$r, out2$rho, tolerance = 0.01)
})


test_that("cor_test distance", {
  skip_if(getRversion() < "4.0")
  skip_if_not_or_load_if_installed("energy")

  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "distance")
  comparison <- energy::dcorT.test(iris$Petal.Length, iris$Petal.Width)
  expect_equal(out$r, as.numeric(comparison$estimate), tolerance = 0.001)
  expect_identical(out$Method, "Distance (Bias Corrected)")
})


test_that("cor_test percentage", {
  skip_if_not_or_load_if_installed("WRS2")

  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "percentage")
  comparison <- WRS2::pbcor(iris$Petal.Length, iris$Petal.Width)
  expect_equal(out$r, as.numeric(comparison$cor), tolerance = 0.01)
})


test_that("cor_test shepherd", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "shepherd")
  expect_equal(out$rho, 0.94762, tolerance = 0.01)

  skip_if_not_or_load_if_installed("BayesFactor")
  set.seed(333)
  out2 <- cor_test(
    iris,
    "Petal.Length",
    "Petal.Width",
    method = "shepherd",
    bayesian = TRUE
  )
  expect_equal(out2$rho, 0.9429992, tolerance = 0.01)
})


test_that("cor_test blomqvist", {
  skip_if_not_or_load_if_installed("wdm")

  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "blomqvist")
  expect_equal(out$r, 0.9066667, tolerance = 0.01)
})

test_that("cor_test hoeffding and somers", {
  skip_if_not_or_load_if_installed("Hmisc")
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "hoeffding")
  expect_equal(out$r, 0.5629277, tolerance = 0.01)

  set.seed(333)
  df <- data.frame(x = 1:6, y = c(0, 0, 1, 0, 1, 1))
  out2 <- cor_test(df, "y", "x", method = "somers")
  expect_equal(out2$Dxy, 0.7777778, tolerance = 0.01)
})

test_that("cor_test gamma", {
  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "gamma")
  expect_equal(out$r, 0.8453925, tolerance = 0.01)
})

test_that("cor_test gaussian", {
  skip_if_not_or_load_if_installed("BayesFactor")

  set.seed(333)
  out <- cor_test(iris, "Petal.Length", "Petal.Width", method = "gaussian")
  expect_equal(out$r, 0.87137, tolerance = 0.01)

  out <- cor_test(
    iris,
    "Petal.Length",
    "Petal.Width",
    method = "gaussian",
    bayesian = TRUE
  )
  expect_equal(out$rho, 0.8620878, tolerance = 0.01)
})


# Additional arguments ----------------------------------------------------

test_that("cor_test one-sided p value", {
  baseline <- cor.test(
    iris$Petal.Length,
    iris$Petal.Width,
    alternative = "greater"
  )

  out <- cor_test(iris, "Petal.Length", "Petal.Width", alternative = "greater")
  expect_equal(out$p, baseline$p.value, tolerance = 0.000001)
})


# Edge cases --------------------------------------------------------------

test_that("cor_test 2 valid observations", {
  out <- suppressWarnings(correlation(data.frame(
    v2 = c(2, 1, 1, 2),
    v3 = c(1, 2, NA, NA)
  )))
  expect_true(is.na(out$r))
})
