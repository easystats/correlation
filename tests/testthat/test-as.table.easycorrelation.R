context("as.table.easycorrelation")



test_that("as.table.easycorrelation", {

  cor <- correlation(iris)
  m <- as.table(cor)
  testthat::expect_equal(c(nrow(m), ncol(m)), c(3, 4))

  library(dplyr)
  cor <- correlation(dplyr::group_by(iris, Species))
  m <- as.table(cor)
  testthat::expect_equal(c(nrow(m), ncol(m)), c(11, 5))
})
