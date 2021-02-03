
test_that("comparison with other packages", {
  if (requireNamespace("ppcor") &
    requireNamespace("Hmisc") &
    requireNamespace("dplyr")) {
    library(dplyr)
    set.seed(333)

    # Pearson
    out <- correlation(iris, include_factors = FALSE)
    rez <- as.data.frame(as.table(out))

    r <- as.matrix(rez[2:5])
    expect_equal(mean(r - cor(iris[1:4])), 0, tolerance = 0.0001)

    hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
    expect_equal(mean(r - hmisc$r), 0, tolerance = 0.0001)

    p <- as.matrix(attributes(rez)$p[2:5])
    expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tolerance = 0.0001)


    # Spearman
    out <- correlation(iris, include_factors = FALSE, method = "spearman")
    rez <- as.data.frame(as.table(out))

    r <- as.matrix(rez[2:5])
    expect_equal(mean(r - cor(iris[1:4], method = "spearman")), 0, tolerance = 0.0001)

    hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("spearman"))
    expect_equal(mean(r - hmisc$r), 0, tolerance = 0.0001)

    p <- as.matrix(attributes(rez)$p[2:5])
    expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tolerance = 0.0001)

    # Kendall
    out <- correlation(iris, include_factors = FALSE, method = "kendall")
    rez <- as.data.frame(as.table(out))

    r <- as.matrix(rez[2:5])
    expect_equal(mean(r - cor(iris[1:4], method = "kendall")), 0, tolerance = 0.0001)

    # Biweight
    out <- correlation(iris, include_factors = FALSE, method = "biweight")
    rez <- as.data.frame(as.table(out))
    r <- as.matrix(rez[2:5])
    expect_equal(mean(r - cor(iris[1:4])), 0, tolerance = 0.01)

    # X and Y
    out <- correlation(iris[1:2], iris[3:4])
    rez <- as.data.frame(as.table(out))
    r <- as.matrix(rez[2:3])
    expect_equal(mean(r - cor(iris[1:2], iris[3:4])), 0, tolerance = 0.0001)

    # Partial
    out <- correlation(mtcars, include_factors = FALSE, partial = TRUE, p_adjust = "none")
    rez <- as.data.frame(as.table(out))

    r <- as.matrix(rez[2:ncol(rez)])
    ppcor <- ppcor::pcor(mtcars)
    expect_equal(max(r - as.matrix(ppcor$estimate)), 0, tolerance = 0.0001)

    p <- as.matrix(attributes(rez)$p[2:ncol(rez)])
    expect_true(mean(abs(p - as.matrix(ppcor$p.value))) < 0.05)


    # Bayesian
    if (requireNamespace("BayesFactor")) {
      out <- correlation(iris, include_factors = FALSE, bayesian = TRUE)
      rez <- as.data.frame(as.table(out))

      r <- as.matrix(rez[2:5])
      expect_equal(mean(r - cor(iris[1:4])), 0, tolerance = 0.01)

      hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
      expect_equal(mean(r - hmisc$r), 0, tolerance = 0.01)

      pd <- as.matrix(attributes(rez)$pd[2:5])
      p <- bayestestR::pd_to_p(pd)
      expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tolerance = 0.01)


      # Bayesian - Partial
      out <- correlation(iris, include_factors = FALSE, bayesian = TRUE, partial = TRUE)
      rez <- as.data.frame(as.table(out))

      r <- as.matrix(rez[2:5])
      ppcor <- ppcor::pcor(iris[1:4])
      expect_equal(max(r - as.matrix(ppcor$estimate)), 0, tolerance = 0.02)

      pd <- as.matrix(attributes(rez)$pd[2:ncol(rez)])
      p <- bayestestR::pd_to_p(pd)
      expect_equal(mean(abs(p - as.matrix(ppcor$p.value))), 0, tolerance = 0.001)


      # Bayesian (Full) - Partial
      out <- correlation(iris, include_factors = FALSE, bayesian = TRUE, partial = TRUE, partial_bayesian = TRUE)
      rez <- as.data.frame(as.table(out))

      r <- as.matrix(rez[2:5])
      ppcor <- ppcor::pcor(iris[1:4])
      expect_equal(max(r - as.matrix(ppcor$estimate)), 0, tolerance = 0.02)
    }
  }
})







# Size
test_that("format checks", {
  out <- correlation(iris, include_factors = TRUE)
  expect_equal(c(nrow(as.table(out)), ncol(as.table(out))), c(7, 8))
  expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(6, 7))

  out <- correlation(iris, method = "auto", include_factors = TRUE)
  expect_equal(c(nrow(as.table(out)), ncol(as.table(out))), c(7, 8))
  expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(6, 7))

  expect_true(all(c("Pearson", "Point-biserial", "Tetrachoric") %in% out$Method))

  # X and Y
  out <- correlation(iris[1:2], iris[3:4])
  expect_equal(c(nrow(out), ncol(out)), c(4, 10))
  expect_equal(c(nrow(as.table(out)), ncol(as.table(out))), c(2, 3))
  expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(2, 3))

  # Grouped
  if (requireNamespace("dplyr")) {
    out <- iris %>%
      dplyr::group_by(Species) %>%
      correlation(include_factors = TRUE)
    expect_equal(c(nrow(out), ncol(out)), c(18, 11))
    expect_equal(c(nrow(as.table(out)), ncol(as.table(out))), c(12, 6))
    expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(9, 5))
  }

  # Bayesian full partial
  if (requireNamespace("BayesFactor")) {
    out <- correlation(iris, include_factors = TRUE, multilevel = TRUE, bayesian = TRUE, partial = TRUE, partial_bayesian = TRUE)
    expect_equal(c(nrow(out), ncol(out)), c(6, 14))
    expect_equal(c(nrow(summary(out, redundant = TRUE)), ncol(summary(out, redundant = TRUE))), c(4, 5))
    expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(3, 4))
  }
})









test_that("specific types", {
  data <- data.frame(
    x = as.ordered(sample(1:5, 20, TRUE)),
    y = as.ordered(sample(letters[1:5], 20, TRUE))
  )

  correlation(data, method = "polychoric")
})
