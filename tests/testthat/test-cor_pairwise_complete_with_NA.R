test_that("pairwise complete correlation with missing data works", {
  set.seed(345345)
  data_set <- data.frame(
    a = sample(c(NA, 1:5), 1000, replace = TRUE),
    b = sample(c(NA, 1:5), 1000, replace = TRUE)
  )

  expected_cor <- cor(data_set, use = "pairwise.complete.obs")

  got_cor <- correlation::correlation(
    data = data_set,
    method = "auto",
    missing = "keep_pairwise"
  )

  testthat::expect_equal(got_cor$r, expected_cor[1, 2])
})
