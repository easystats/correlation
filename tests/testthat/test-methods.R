test_that("as.matrix.correlation", {
  rez <- correlation(mtcars)
  m <- as.matrix(rez)
  expect_equal(dim(m), c(11, 11))
})

test_that("summary.correlation - target column", {
  expect_snapshot(summary(correlation(ggplot2::msleep), target_col = "t"))
  expect_snapshot(summary(correlation(ggplot2::msleep), target_col = "df_error"))
  expect_snapshot(summary(correlation(ggplot2::msleep), target_col = "p"))
  expect_error(summary(correlation(ggplot2::msleep), target_col = "not_a_column_name"))
})
