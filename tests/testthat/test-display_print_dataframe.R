test_that("display and print method works - markdown", {
  skip_on_cran()
  skip_if(getRversion() < "4.0.0")
  skip_if_not_or_load_if_installed("gt")

  expect_snapshot(display(correlation(iris)))
})

# display and print method works - HTML -----------------------------

test_that("display and print method works - HTML", {
  skip_on_cran()
  skip_if(getRversion() < "4.0.0")
  skip_if_not_or_load_if_installed("gt")

  expect_s3_class(display(correlation(subset(mtcars, select = c("wt", "mpg"))), format = "html"), "gt_tbl")
  expect_s3_class(print_html(correlation(subset(mtcars, select = c("wt", "mpg")))), "gt_tbl")
})
