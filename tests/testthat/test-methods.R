test_that("as.matrix.correlation", {
  rez <- correlation(mtcars)
  m <- as.matrix(rez)
  expect_equal(dim(m), c(11, 11))
})

test_that("summary.correlation - target column", {
  skip_if_not_or_load_if_installed("ggplot2")
  expect_snapshot(summary(correlation(ggplot2::msleep), target = "t"))
  expect_snapshot(summary(correlation(ggplot2::msleep), target = "df_error"))
  expect_snapshot(summary(correlation(ggplot2::msleep), target = "p"))
  expect_error(summary(
    correlation(ggplot2::msleep),
    target = "not_a_column_name"
  ))
})
