test_that("as.list", {
  skip_if_not_or_load_if_installed("poorman")
  skip_if_not_or_load_if_installed("ggplot2")

  # no groups
  set.seed(123)
  expect_snapshot(as.list(correlation(mtcars)))

  # with groups
  set.seed(123)
  expect_snapshot(suppressWarnings(
    as.list(msleep %>%
      group_by(vore) %>%
      correlation(method = "spearman"))
  ))

  expect_snapshot(suppressWarnings(
    as.list(mtcars %>%
      group_by(am) %>%
      correlation(
        select = c("cyl", "wt"),
        select2 = "hp",
        method = "percentage"
      ))
  ))
})
