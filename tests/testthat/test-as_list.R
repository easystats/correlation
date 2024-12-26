test_that("as.list", {
  skip_if_not_or_load_if_installed("datawizard")
  skip_if_not_or_load_if_installed("ggplot2")

  # no groups
  set.seed(123)
  out <- as.list(correlation(mtcars))
  expect_snapshot(print(out, table_width = Inf))

  # with groups
  set.seed(123)
  data(msleep, package = "ggplot2")
  out <- as.list(
    correlation(datawizard::data_group(msleep, "vore"), method = "spearman")
  )
  expect_snapshot(print(out, table_width = Inf))

  out <- as.list(
    correlation(
      datawizard::data_group(mtcars, "am"),
      select = c("cyl", "wt"),
      select2 = "hp",
      method = "percentage"
    )
  )
  expect_snapshot(print(out, table_width = Inf))
})
