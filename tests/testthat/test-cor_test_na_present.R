test_that("cor_test frequentist", {
  skip_if_not_or_load_if_installed("ggplot2")

  expect_error(cor_test(brainwt, sleep_rem, data = ggplot2::msleep))

  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep)
  expect_equal(out$r, -0.2213348, tolerance = 0.01)
})

test_that("cor_test inputs are columns from data.frame and/or vector", {
  skip_if_not_or_load_if_installed("ggplot2")
  x <- ggplot2::msleep$brainwt
  y <- ggplot2::msleep$sleep_rem

  expect_error(cor_test("brainwt", "sleep_rem"))
  expect_error(cor_test(x, "sleep_rem"))

  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep)
  out2 <- cor_test(x, "sleep_rem", data = ggplot2::msleep)
  out3 <- cor_test("brainwt", y, data = ggplot2::msleep)
  out4 <- cor_test(x, y)

  expect_equal(out$r, out2$r, tolerance = 0.001)
  expect_equal(out$CI_low, out2$CI_low, tolerance = 0.001)
  expect_equal(out$CI_high, out2$CI_high, tolerance = 0.001)
  expect_equal(out$r, out3$r, tolerance = 0.001)
  expect_equal(out$CI_low, out3$CI_low, tolerance = 0.001)
  expect_equal(out$CI_high, out3$CI_high, tolerance = 0.001)
  expect_equal(out$r, out4$r, tolerance = 0.001)
  expect_equal(out$CI_low, out4$CI_low, tolerance = 0.001)
  expect_equal(out$CI_high, out4$CI_high, tolerance = 0.001)
})

test_that("cor_test kendall tau-a", {
  skip_if_not_or_load_if_installed("ggplot2")
  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "kendall", tau_type = "a")
  completeCase <- stats::complete.cases(ggplot2::msleep[["brainwt"]], ggplot2::msleep[["sleep_rem"]])
  var_x <- ggplot2::msleep[["brainwt"]][completeCase]
  var_y <- ggplot2::msleep[["sleep_rem"]][completeCase]
  tab <- table(var_x, var_y)
  n <- sum(tab)
  ConDisParams <- DescTools::ConDisPairs(tab)[3:4]
  z <- (ConDisParams$C - ConDisParams$D) / sqrt(n * (n - 1) * (2 * n + 5) / 18)

  expect_equal(out$tau, (ConDisParams$C - ConDisParams$D)/(n * (n - 1)/2), tolerance = 0.001)
  expect_equal(out$p, 2 * stats::pnorm(abs(z), lower.tail = FALSE), tolerance = 0.001)
})

test_that("cor_test kendall tau-b", {
  # error due to stats::cor.test: `Cannot compute exact p-value with ties`
  skip_if_not_or_load_if_installed("ggplot2")

  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "kendall")
  out2 <- stats::cor.test(ggplot2::msleep$brainwt, ggplot2::msleep$sleep_rem, method = "kendall")

  expect_equal(out$tau, out2$estimate[[1]], tolerance = 0.001)
  expect_equal(out$p, out2$p.value[[1]], tolerance = 0.001)
})

test_that("cor_test kendall tau-c", {
  # error due to stats::cor.test: `Cannot compute exact p-value with ties`
  skip_if_not_or_load_if_installed("ggplot2")
  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "kendall", tau_type = "c")
  out2 <- stats::cor.test(ggplot2::msleep$brainwt, ggplot2::msleep$sleep_rem, method = "kendall")

  completeCase <- stats::complete.cases(ggplot2::msleep[["brainwt"]], ggplot2::msleep[["sleep_rem"]])
  var_x <- ggplot2::msleep[["brainwt"]][completeCase]
  var_y <- ggplot2::msleep[["sleep_rem"]][completeCase]
  tab <- table(var_x, var_y)
  ConDisParams <- DescTools::ConDisPairs(tab)[3:4]

  expect_equal(out$tau, (ConDisParams$C - ConDisParams$D) * 2 * min(dim(tab))/(sum(tab)^2 * (min(dim(tab)) - 1)), tolerance = 0.001)
  expect_equal(out$p, out2$p.value[[1]], tolerance = 0.001)
})

test_that("cor_test bayesian", {
  skip_if_not_or_load_if_installed("ggplot2")
  skip_if_not_or_load_if_installed("BayesFactor")

  set.seed(123)
  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, bayesian = TRUE)
  expect_equal(out$r, -0.1947696, tolerance = 0.01)
})

test_that("cor_test tetrachoric", {
  # warning due to polycor::polyserial: `initial correlation inadmissable...`
  skip_if_not_or_load_if_installed("psych")
  skip_if_not_or_load_if_installed("polycor")
  skip_if_not_or_load_if_installed("ggplot2")

  data <- ggplot2::msleep
  data$brainwt_binary <- ifelse(data$brainwt > 0.7, 1, 0)
  data$sleep_rem_binary <- ifelse(data$sleep_rem > 1.5, 1, 0)

  # With Factors / Binary
  out <- cor_test("brainwt_binary", "sleep_rem_binary", data = data, method = "tetrachoric")
  expect_equal(out$r, 0.1599637, tolerance = 0.01)

  data$sleep_rem_ordinal <- as.factor(round(data$sleep_rem))
  data$brainwt_ordinal <- as.factor(round(data$brainwt))
  out <- cor_test("brainwt", "brainwt_ordinal", data = data, method = "polychoric")
  expect_equal(out$r, 0.9999, tolerance = 0.01)

  # Biserial
  expect_error(cor_test("brainwt", "sleep_rem", data = data, method = "biserial"))

  out <- cor_test("brainwt", "sleep_rem_binary", data = data, method = "pointbiserial")
  expect_equal(out$r, -0.1577557, tolerance = 0.0001)

  out <- cor_test("brainwt", "sleep_rem_binary", data = data, method = "biserial")
  expect_equal(out$r, -0.1957441, tolerance = 0.0001)

  out <- cor_test("brainwt", "sleep_rem_binary", data = data, method = "rankbiserial")
  expect_equal(out$r, -0.002991068, tolerance = 0.0001)
})

test_that("cor_test robust", {
  skip_if_not_or_load_if_installed("ggplot2")

  out1 <- cor_test(datawizard::ranktransform(ggplot2::msleep$brainwt, sign = FALSE, method = "average"), datawizard::ranktransform(ggplot2::msleep$sleep_rem, sign = FALSE, method = "average"))
  out2 <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "spearman")
  expect_equal(out1$r, out2$r, tolerance = 0.01)
})

test_that("cor_test distance", {
  skip_if_not_or_load_if_installed("ggplot2")
  skip_if_not_or_load_if_installed("energy")
  skip_if_not_or_load_if_installed("poorman")

  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "distance")
  df <- poorman::filter(ggplot2::msleep, !is.na(brainwt), !is.na(sleep_rem))
  comparison <- energy::dcorT.test(df$brainwt, df$sleep_rem)
  expect_equal(out$r, as.numeric(comparison$estimate), tolerance = 0.01)
})

test_that("cor_test percentage", {
  skip_if_not_or_load_if_installed("ggplot2")
  skip_if_not_or_load_if_installed("WRS2")

  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "percentage")
  comparison <- WRS2::pbcor(ggplot2::msleep$brainwt, ggplot2::msleep$sleep_rem)
  expect_equal(out$r, as.numeric(comparison$cor), tolerance = 0.01)
})

test_that("cor_test shepherd", {
  skip_if_not_or_load_if_installed("ggplot2")

  set.seed(333)
  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "shepherd")
  expect_equal(out$r, -0.4480401, tolerance = 0.01)

  skip_if_not_or_load_if_installed("BayesFactor")
  set.seed(333)
  out2 <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "shepherd", bayesian = TRUE)
  expect_equal(out2$r, -0.3978831, tolerance = 0.01)
})

test_that("cor_test blomqvist", {
  skip_if_not_or_load_if_installed("ggplot2")
  skip_if_not_or_load_if_installed("wdm")

  set.seed(333)
  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "blomqvist")
  expect_equal(out$r, -0.4681911, tolerance = 0.01)
})

test_that("cor_test hoeffding", {
  skip_if_not_or_load_if_installed("ggplot2")
  skip_if_not_or_load_if_installed("Hmisc")

  set.seed(333)
  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "hoeffding")
  expect_equal(out$r, 0.04427718, tolerance = 0.01)
})

test_that("cor_test gamma", {
  skip_if_not_or_load_if_installed("ggplot2")

  set.seed(333)
  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "gamma")
  expect_equal(out$r, -0.2675799, tolerance = 0.01)
})

test_that("cor_test gaussian", {
  skip_if_not_or_load_if_installed("ggplot2")

  set.seed(333)
  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "gaussian")
  expect_equal(out$r, -0.3679795, tolerance = 0.01)

  skip_if_not_or_load_if_installed("BayesFactor")
  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, method = "gaussian", bayesian = TRUE)
  expect_equal(out$r, -0.3269572, tolerance = 0.01)
})

# Additional arguments ----------------------------------------------------

test_that("cor_test one-sided p value", {
  skip_if_not_or_load_if_installed("ggplot2")

  baseline <- cor.test(ggplot2::msleep$brainwt, ggplot2::msleep$sleep_rem, alternative = "greater")

  out <- cor_test("brainwt", "sleep_rem", data = ggplot2::msleep, alternative = "greater")
  expect_equal(out$p, baseline$p.value, tolerance = 0.000001)
})
