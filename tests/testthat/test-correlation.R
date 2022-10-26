.runThisTest <- Sys.getenv("RunAllcorrelationTests") == "yes"

test_that("comparison with other packages", {
  skip_if_not_installed("ppcor")
  skip_if_not_installed("Hmisc")
  skip_if_not_installed("lme4")

  set.seed(333)

  # Pearson
  out <- correlation(iris, include_factors = FALSE)
  rez <- as.data.frame(summary(out, redundant = TRUE))

  r <- as.matrix(rez[2:5])
  expect_equal(mean(r - cor(iris[1:4])), 0, tolerance = 0.0001)

  hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
  expect_equal(mean(r - hmisc$r), 0, tolerance = 0.0001)

  p <- as.matrix(attributes(rez)$p[2:5])
  expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tolerance = 0.0001)


  # Spearman
  out <- correlation(iris, include_factors = FALSE, method = "spearman")
  rez <- as.data.frame(summary(out, redundant = TRUE))

  r <- as.matrix(rez[2:5])
  expect_equal(mean(r - cor(iris[1:4], method = "spearman")), 0, tolerance = 0.0001)

  hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("spearman"))
  expect_equal(mean(r - hmisc$r), 0, tolerance = 0.0001)

  p <- as.matrix(attributes(rez)$p[2:5])
  expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tolerance = 0.0001)

  # Kendall
  out <- correlation(iris, include_factors = FALSE, method = "kendall")
  rez <- as.data.frame(summary(out, redundant = TRUE))

  r <- as.matrix(rez[2:5])
  expect_equal(mean(r - cor(iris[1:4], method = "kendall")), 0, tolerance = 0.0001)

  # Biweight
  out <- correlation(iris, include_factors = FALSE, method = "biweight")
  rez <- as.data.frame(summary(out, redundant = TRUE))
  r <- as.matrix(rez[2:5])
  expect_equal(mean(r - cor(iris[1:4])), 0, tolerance = 0.01)

  # X and Y
  out <- correlation(iris[1:2], iris[3:4])
  rez <- as.data.frame(summary(out, redundant = TRUE))
  r <- as.matrix(rez[2:3])
  expect_equal(mean(r - cor(iris[1:2], iris[3:4])), 0, tolerance = 0.0001)

  # Partial
  out <- correlation(mtcars, include_factors = FALSE, partial = TRUE, p_adjust = "none")
  rez <- as.data.frame(summary(out, redundant = TRUE))

  r <- as.matrix(rez[2:ncol(rez)])
  ppcor <- ppcor::pcor(mtcars)
  expect_equal(max(r - as.matrix(ppcor$estimate)), 0, tolerance = 0.0001)

  p <- as.matrix(attributes(rez)$p[2:ncol(rez)])
  expect_true(mean(abs(p - as.matrix(ppcor$p.value))) < 0.05)

  # Bayesian
  if (.runThisTest) {
    out <- correlation(iris, include_factors = FALSE, bayesian = TRUE)
    rez <- as.data.frame(summary(out, redundant = TRUE))

    r <- as.matrix(rez[2:5])
    expect_equal(mean(r - cor(iris[1:4])), 0, tolerance = 0.01)

    hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
    expect_equal(mean(r - hmisc$r), 0, tolerance = 0.01)

    pd <- as.matrix(attributes(rez)$pd[2:5])
    p <- bayestestR::pd_to_p(pd)
    expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tolerance = 0.01)


    # Bayesian - Partial
    out <- correlation(iris, include_factors = FALSE, bayesian = TRUE, partial = TRUE)
    rez <- as.data.frame(summary(out, redundant = TRUE))

    r <- as.matrix(rez[2:5])
    ppcor <- ppcor::pcor(iris[1:4])
    expect_equal(max(r - as.matrix(ppcor$estimate)), 0, tolerance = 0.02)

    pd <- as.matrix(attributes(rez)$pd[2:ncol(rez)])
    p <- bayestestR::pd_to_p(pd)
    expect_equal(mean(abs(p - as.matrix(ppcor$p.value))), 0, tolerance = 0.001)


    # Bayesian (Full) - Partial
    out <- correlation(iris, include_factors = FALSE, bayesian = TRUE, partial = TRUE, partial_bayesian = TRUE)
    rez <- as.data.frame(summary(out, redundant = TRUE))

    r <- as.matrix(rez[2:5])
    ppcor <- ppcor::pcor(iris[1:4])
    expect_equal(max(r - as.matrix(ppcor$estimate)), 0, tolerance = 0.02)
  }
})



# Size
test_that("format checks", {
  skip_if_not_installed("psych")

  out <- correlation(iris, include_factors = TRUE)
  expect_equal(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(7, 8))
  expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(6, 7))

  out <- correlation(iris, method = "auto", include_factors = TRUE)
  expect_equal(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(7, 8))
  expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(6, 7))

  expect_true(all(c("Pearson correlation", "Point-biserial correlation", "Tetrachoric correlation") %in% out$Method))

  # X and Y
  out <- correlation(iris[1:2], iris[3:4])
  expect_equal(c(nrow(out), ncol(out)), c(4, 11))
  expect_equal(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(2, 3))
  expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(2, 3))

  # Grouped
  skip_if_not_installed("poorman")
  library(poorman)

  out <- iris %>%
    group_by(Species) %>%
    correlation(include_factors = TRUE)
      expect_equal(c(nrow(out), ncol(out)), c(18, 12))
      expect_equal(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(12, 6))
      expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(9, 5))

  # pipe and select
  out <- iris %>%
    correlation(
      select = "Petal.Width",
      select2 = c("Sepal.Length", "Sepal.Width")
    )
  expect_equal(c(nrow(out), ncol(out)), c(2, 11))
  expect_equal(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(1, 3))
  expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(1, 3))
  expect_equal(out[["r"]], c(0.8179, -0.3661), tolerance = 1e-2)
  expect_equal(out$Parameter1, c("Petal.Width", "Petal.Width"))
  expect_equal(out$Parameter2, c("Sepal.Length", "Sepal.Width"))

  # Bayesian full partial
  skip_if_not_installed("BayesFactor")
  skip_if_not_installed("lme4")

  if (.runThisTest) {
    out <- correlation(
      iris,
      include_factors = TRUE,
      multilevel = TRUE,
      bayesian = TRUE,
      partial = TRUE,
      partial_bayesian = TRUE
    )
    expect_equal(c(nrow(out), ncol(out)), c(6, 14))
    expect_equal(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(4, 5))
    expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(3, 4))
  }
})



test_that("specific types", {
  skip_on_cran()
  skip_if_not_installed("psych")

  data <- data.frame(
    x = as.ordered(sample(1:5, 20, TRUE)),
    y = as.ordered(sample(letters[1:5], 20, TRUE))
  )

  correlation(data, method = "polychoric")
})

test_that("correlation doesn't fail when BFs are NA", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("BayesFactor")

  df <- ggplot2::msleep

  set.seed(123)
  df_corr <- correlation(subset(df, vore == "carni"), bayesian = TRUE)
  expect_equal(nrow(df_corr), 15L)
})

test_that("as.data.frame for correlation output", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")

  set.seed(123)
  expect_snapshot(as.data.frame(correlation(ggplot2::msleep)))
})
