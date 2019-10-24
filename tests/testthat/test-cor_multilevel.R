context("multilevel correlations")



test_that("comparison rmcorr", {
  rez_rmcorr <- rmcorr::rmcorr(Species, Sepal.Length, Sepal.Width, dataset = iris)
  rez <- cor_test(iris[c(1, 2, 5)], "Sepal.Length", "Sepal.Width", partial = TRUE, multilevel = TRUE)

  testthat::expect_equal(rez$r, rez_rmcorr$r, tol = 0.001)
  testthat::expect_equal(rez$p, rez_rmcorr$p, tol = 0.001)
})




test_that("Reductio ad absurdum", {
  cormatrix <- matrix(c(
    1.0, 0.3, 0.6,
    0.3, 1.0, 0.0,
    0.6, 0.0, 1.0
  ), nrow = 3)

  data <- bayestestR::simulate_correlation(n = 500, r = cormatrix)
  # Add factor levels "at random", so the grouping structure should NOT change much
  data$Group <- sample(rep_len(c("A", "B", "C"), length.out = 500))

  rez <- correlation(data)
  testthat::expect_equal(max(as.matrix(rez) - cormatrix), 0, tol = 0.000001)

  rez <- correlation(data, multilevel = TRUE)
  testthat::expect_equal(max(as.matrix(rez) - cormatrix), 0, tol = 0.002)

  rez <- correlation(data, multilevel = TRUE, partial = TRUE)
  testthat::expect_equal(max(as.matrix(pcor_to_cor(rez)) - cormatrix), 0, tol = 0.002)
})
