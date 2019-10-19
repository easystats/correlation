context("correlation")



test_that("correlation basic [comparison with other packages]", {
  library(dplyr)

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
})

  # Size



  # testthat::expect_equal(nrow(out), 16)
  #
  # out <- correlation(iris, bayesian = TRUE)
  # testthat::expect_equal(nrow(out), 16)
  #
  # out <- correlation(iris, partial = TRUE)
  # testthat::expect_equal(nrow(out), 16)
  #
  # out <- correlation(iris, partial = "semi")
  # testthat::expect_equal(nrow(out), 16)
  # testthat::expect_equal(length(as.list(out)), 7)
  #
  # out <- iris %>%
  #   select(starts_with("Sepal")) %>%
  #   correlation(iris %>%
  #     select(starts_with("Petal")))
  # testthat::expect_equal(nrow(out), 4)
  #
  # out <- iris %>%
  #   group_by(Species) %>%
  #   correlation()
  # testthat::expect_equal(nrow(out), 48)
  #
  # out <- iris %>%
  #   group_by(Species) %>%
  #   select(Species, starts_with("Petal")) %>%
  #   correlation(iris %>%
  #     group_by(Species) %>%
  #     select(Species, starts_with("Sepal")))
  # testthat::expect_equal(nrow(out), 12)
  #
  # testthat::expect_error(iris %>%
  #   group_by(Species) %>%
  #   correlation(iris))

