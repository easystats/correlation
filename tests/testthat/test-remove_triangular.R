context("remove_triangular")



test_that("remove_triangular", {
  cor <- remove_triangular(correlation(iris))

  testthat::expect_equal(nrow(cor), 6)
})
