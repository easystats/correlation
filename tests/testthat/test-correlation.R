test_that("comparison with other packages", {
  skip_if_not_or_load_if_installed("ppcor")
  skip_if_not_or_load_if_installed("Hmisc")
  skip_if_not_or_load_if_installed("lme4")
  skip_if_not_or_load_if_installed("BayesFactor")

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
})



# Size
test_that("format checks", {
  skip_if_not_or_load_if_installed("psych")

  out <- correlation(iris, include_factors = TRUE)
  expect_identical(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(7L, 8L))
  expect_identical(c(nrow(summary(out)), ncol(summary(out))), c(6L, 7L))

  expect_message(
    out <- correlation(iris, method = "auto", include_factors = TRUE),
    "Check your data"
  )
  expect_identical(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(7L, 8L))
  expect_identical(c(nrow(summary(out)), ncol(summary(out))), c(6L, 7L))

  expect_true(all(c("Pearson correlation", "Point-biserial correlation", "Tetrachoric correlation") %in% out$Method))

  # X and Y
  out <- correlation(iris[1:2], iris[3:4])
  expect_identical(c(nrow(out), ncol(out)), c(4L, 11L))
  expect_identical(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(2L, 3L))
  expect_identical(c(nrow(summary(out)), ncol(summary(out))), c(2L, 3L))

  # Grouped
  skip_if_not_or_load_if_installed("poorman")
  library(poorman) # loading the library is necessary to use the pipe operator

  out <- iris %>%
    group_by(Species) %>%
    correlation(include_factors = TRUE)
  expect_identical(c(nrow(out), ncol(out)), c(18L, 12L))
  expect_identical(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(12L, 6L))
  expect_identical(c(nrow(summary(out)), ncol(summary(out))), c(9L, 5L))

  # pipe and select
  out <- iris %>%
    correlation(
      select = "Petal.Width",
      select2 = c("Sepal.Length", "Sepal.Width")
    )
  expect_identical(c(nrow(out), ncol(out)), c(2L, 11L))
  expect_identical(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(1L, 3L))
  expect_identical(c(nrow(summary(out)), ncol(summary(out))), c(1L, 3L))
  expect_equal(out[["r"]], c(0.8179, -0.3661), tolerance = 1e-2)
  expect_identical(out$Parameter1, c("Petal.Width", "Petal.Width"))
  expect_identical(out$Parameter2, c("Sepal.Length", "Sepal.Width"))

  # Bayesian full partial
  skip_if_not_or_load_if_installed("BayesFactor")
  skip_if_not_or_load_if_installed("lme4")


  out <- correlation(
    iris,
    include_factors = TRUE,
    multilevel = TRUE,
    bayesian = TRUE,
    partial = TRUE,
    partial_bayesian = TRUE
  )
  expect_identical(c(nrow(out), ncol(out)), c(6L, 14L))
  expect_identical(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(4L, 5L))
  expect_identical(c(nrow(summary(out)), ncol(summary(out))), c(3L, 4L))
})


test_that("specific types", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("psych")

  data <- data.frame(
    x = as.ordered(sample(1:5, 20, TRUE)),
    y = as.ordered(sample(letters[1:5], 20, TRUE))
  )

  expect_warning(correlation(data, method = "polychoric"), regex = "It seems like")
})

test_that("correlation doesn't fail when BFs are NA", {
  skip_if_not_or_load_if_installed("ggplot2")
  skip_if_not_or_load_if_installed("BayesFactor")

  df <- ggplot2::msleep

  set.seed(123)
  df_corr <- correlation(subset(df, vore == "carni"), bayesian = TRUE)
  expect_identical(nrow(df_corr), 15L)
})

test_that("as.data.frame for correlation output", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("ggplot2")

  set.seed(123)
  expect_snapshot(as.data.frame(correlation(ggplot2::msleep)))
})
