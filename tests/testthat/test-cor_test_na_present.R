test_that("cor_test frequentist", {
  skip_if_not_or_load_if_installed("ggplot2")

  expect_error(cor_test(ggplot2::msleep, brainwt, sleep_rem))

  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem")
  expect_equal(out$r, -0.2213348, tolerance = 0.01)
})

test_that("cor_test kendall", {
  skip_if_not_or_load_if_installed("ggplot2")

  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "kendall")
  out2 <- suppressWarnings(stats::cor.test(ggplot2::msleep$brainwt, ggplot2::msleep$sleep_rem, method = "kendall"))

  expect_equal(out$tau, out2$estimate[[1]], tolerance = 0.001)
  expect_equal(out$p, out2$p.value[[1]], tolerance = 0.001)
})

test_that("cor_test bayesian", {
  skip_if_not_or_load_if_installed("BayesFactor")

  set.seed(123)
  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", bayesian = TRUE)
  expect_equal(out$rho, -0.1947696, tolerance = 0.01)
})

test_that("cor_test tetrachoric", {
  skip_if_not_or_load_if_installed("psych")
  skip_if_not_or_load_if_installed("polycor")
  skip_if_not_or_load_if_installed("ggplot2")

  data <- ggplot2::msleep
  data$brainwt_binary <- as.numeric(data$brainwt > 3)
  data$sleep_rem_binary <- as.numeric(data$sleep_rem > 1.2)

  # With Factors / Binary
  expect_error(cor_test(data, "brainwt_binary", "sleep_rem_binary", method = "tetrachoric"))

  data$sleep_rem_ordinal <- as.factor(round(data$sleep_rem))
  data$brainwt_ordinal <- as.factor(round(data$brainwt))

  out <- suppressWarnings(cor_test(data, "brainwt", "brainwt_ordinal", method = "polychoric"))
  expect_equal(out$rho, 0.9999, tolerance = 0.01)

  # Biserial
  expect_error(cor_test(data, "brainwt", "sleep_rem_binary", method = "pointbiserial"))

  expect_error(cor_test(data, "brainwt", "sleep_rem_binary", method = "biserial"))
})


test_that("cor_test robust", {
  skip_if_not_or_load_if_installed("ggplot2")

  out1 <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "pearson", ranktransform = TRUE)
  out2 <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "spearman", ranktransform = FALSE)
  expect_equal(out1$r, out2$rho, tolerance = 0.01)
})


test_that("cor_test distance", {
  skip_if_not_or_load_if_installed("ggplot2")
  skip_if_not_or_load_if_installed("energy")
  skip_if_not_or_load_if_installed("poorman")

  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "distance")
  df <- poorman::filter(ggplot2::msleep, !is.na(brainwt), !is.na(sleep_rem))
  comparison <- energy::dcorT.test(df$brainwt, df$sleep_rem)
  expect_equal(out$r, as.numeric(comparison$estimate), tolerance = 0.01)
})


test_that("cor_test percentage", {
  skip_if_not_or_load_if_installed("ggplot2")
  skip_if_not_or_load_if_installed("WRS2")

  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "percentage")
  comparison <- WRS2::pbcor(ggplot2::msleep$brainwt, ggplot2::msleep$sleep_rem)
  expect_equal(out$r, as.numeric(comparison$cor), tolerance = 0.01)
})


test_that("cor_test shepherd", {
  skip_if_not_or_load_if_installed("ggplot2")

  set.seed(333)
  expect_error(cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "shepherd"))
})


test_that("cor_test blomqvist", {
  skip_if_not_or_load_if_installed("wdm")

  set.seed(333)
  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "blomqvist")
  expect_equal(out$r, -0.4583333, tolerance = 0.01)
})

test_that("cor_test hoeffding", {
  skip_if_not_or_load_if_installed("Hmisc")

  set.seed(333)
  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "hoeffding")
  expect_equal(out$r, 0.04427718, tolerance = 0.01)
})

test_that("cor_test gamma", {
  skip_if_not_or_load_if_installed("ggplot2")

  set.seed(333)
  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "gamma")
  expect_equal(out$r, -0.2675799, tolerance = 0.01)
})

test_that("cor_test gaussian", {
  skip_if_not_or_load_if_installed("ggplot2")

  set.seed(333)
  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "gaussian")
  expect_equal(out$r, -0.3679795, tolerance = 0.01)

  skip_if_not_or_load_if_installed("BayesFactor")
  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", method = "gaussian", bayesian = TRUE)
  expect_equal(out$rho, -0.3269572, tolerance = 0.01)
})


# Additional arguments ----------------------------------------------------


test_that("cor_test one-sided p value", {
  skip_if_not_or_load_if_installed("ggplot2")

  baseline <- cor.test(ggplot2::msleep$brainwt, ggplot2::msleep$sleep_rem, alternative = "greater")

  out <- cor_test(ggplot2::msleep, "brainwt", "sleep_rem", alternative = "greater")
  expect_equal(out$p, baseline$p.value, tolerance = 0.000001)
})
