context("correlation")



test_that("comparison with other packages", {
  library(dplyr)
  set.seed(333)

  # Pearson
  out <- correlation(iris, include_factors = FALSE)
  rez <- as.data.frame(as.table(out))

  r <- as.matrix(rez[2:5])
  testthat::expect_equal(mean(r - cor(iris[1:4])), 0, tol= 0.0001)

  hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
  testthat::expect_equal(mean(r - hmisc$r), 0, tol= 0.0001)

  p <- as.matrix(attributes(rez)$p[2:5])
  testthat::expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tol= 0.0001)


  # Spearman
  out <- correlation(iris, include_factors = FALSE, method = "spearman")
  rez <- as.data.frame(as.table(out))

  r <- as.matrix(rez[2:5])
  testthat::expect_equal(mean(r - cor(iris[1:4], method = "spearman")), 0, tol= 0.0001)

  hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("spearman"))
  testthat::expect_equal(mean(r - hmisc$r), 0, tol= 0.0001)

  p <- as.matrix(attributes(rez)$p[2:5])
  testthat::expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tol= 0.0001)

  # Kendall
  out <- correlation(iris, include_factors = FALSE, method = "kendall")
  rez <- as.data.frame(as.table(out))

  r <- as.matrix(rez[2:5])
  testthat::expect_equal(mean(r - cor(iris[1:4], method = "kendall")), 0, tol= 0.0001)

  # Biweight
  out <- correlation(iris, include_factors = FALSE, method = "biweight")
  rez <- as.data.frame(as.table(out))
  r <- as.matrix(rez[2:5])
  testthat::expect_equal(mean(r - cor(iris[1:4])), 0, tol= 0.01)

  # X and Y
  out <- correlation(iris[1:2], iris[3:4])
  rez <- as.data.frame(as.table(out))
  r <- as.matrix(rez[2:3])
  testthat::expect_equal(mean(r - cor(iris[1:2], iris[3:4])), 0, tol= 0.0001)

  # Partial
  out <- correlation(mtcars, include_factors = FALSE, partial = TRUE, p_adjust = "none")
  rez <- as.data.frame(as.table(out))

  r <- as.matrix(rez[2:ncol(rez)])
  ppcor <- ppcor::pcor(mtcars)
  testthat::expect_equal(max(r - as.matrix(ppcor$estimate)), 0, tol= 0.0001)

  p <- as.matrix(attributes(rez)$p[2:ncol(rez)])
  testthat::expect_true(mean(abs(p - as.matrix(ppcor$p.value))) < 0.05)


  # Bayesian
  out <- correlation(iris, include_factors = FALSE, bayesian = TRUE)
  rez <- as.data.frame(as.table(out))

  r <- as.matrix(rez[2:5])
  testthat::expect_equal(mean(r - cor(iris[1:4])), 0, tol= 0.01)

  hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
  testthat::expect_equal(mean(r - hmisc$r), 0, tol= 0.01)

  pd <- as.matrix(attributes(rez)$pd[2:5])
  p <- bayestestR::pd_to_p(pd)
  testthat::expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tol= 0.01)


  # Bayesian - Partial
  out <- correlation(iris, include_factors = FALSE, bayesian = TRUE, partial = TRUE)
  rez <- as.data.frame(as.table(out))

  r <- as.matrix(rez[2:5])
  ppcor <- ppcor::pcor(iris[1:4])
  testthat::expect_equal(max(r - as.matrix(ppcor$estimate)), 0, tol= 0.02)

  pd <- as.matrix(attributes(rez)$p[2:ncol(rez)])
  p <- bayestestR::pd_to_p(pd)
  testthat::expect_equal(mean(abs(p - as.matrix(ppcor$p.value))), 0, tol= 0.001)

})







  # Size
test_that("format checks", {
  out <- correlation(iris)
  testthat::expect_equal(c(nrow(as.table(out)), ncol(as.table(out))), c(7, 8))
  testthat::expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(6, 7))

  out <- correlation(iris, method = "auto")
  testthat::expect_equal(c(nrow(as.table(out)), ncol(as.table(out))), c(7, 8))
  testthat::expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(6, 7))

  testthat::expect_true(all(c("Pearson", "Biserial", "Tetrachoric") %in% out$Method))

  # X and Y
  out <- correlation(iris[1:2], iris[3:4])
  testthat::expect_equal(c(nrow(out), ncol(out)), c(4, 9))
  testthat::expect_equal(c(nrow(as.table(out)), ncol(as.table(out))), c(2, 3))
  testthat::expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(2, 3))

  # Grouped
  out <- iris %>%
    group_by(Species) %>%
    correlation()
  testthat::expect_equal(c(nrow(out), ncol(out)), c(18, 10))
  testthat::expect_equal(c(nrow(as.table(out)), ncol(as.table(out))), c(12, 6))
  testthat::expect_equal(c(nrow(summary(out)), ncol(summary(out))), c(9, 5))

})





