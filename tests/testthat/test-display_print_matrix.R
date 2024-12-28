# display and print method works - markdown -----------------------------

test_that("display and print method works - markdown", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("gt")
  skip_if_not_or_load_if_installed("poorman")
  expect_snapshot(display(summary(correlation(iris))))
  expect_snapshot(print(summary(correlation(iris))))
})

# display and print method works - html -----------------------------
test_that("display and print method works - html", {
  skip_on_cran()
  skip_if_not_or_load_if_installed("gt")
  expect_s3_class(print_html(summary(correlation(iris))), "gt_tbl")
})

test_that("as.matrix works", {
  skip_if_not_or_load_if_installed("gt")
  skip_if_not_installed("datawizard")
  skip_if(getRversion() < "4.1.0")
  set.seed(123)
  mat1 <- select(mtcars, am, wt, hp) |>
    correlation() |>
    as.matrix()
  set.seed(123)
  mat2 <- select(mtcars, am, wt, hp) |>
    datawizard::data_group(am) |>
    correlation() |>
    as.matrix()
  expect_snapshot(list(mat1, mat2))
})
