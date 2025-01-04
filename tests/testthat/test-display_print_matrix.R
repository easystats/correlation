# display and print method works - markdown -----------------------------

test_that("display and print method works - markdown", {
  skip_on_cran()
  out <- capture.output(display(summary(correlation(iris))))
  expect_identical(
    out[1:7],
    c(
      "[1] \"Table: Correlation Matrix (pearson-method)\"                ",
      "[2] \"\"                                                          ",
      "[3] \"|Parameter    | Petal.Width | Petal.Length | Sepal.Width |\"",
      "[4] \"|:------------|:-----------:|:------------:|:-----------:|\"",
      "[5] \"|Sepal.Length |     0.82*** |      0.87*** |       -0.12 |\"",
      "[6] \"|Sepal.Width  |    -0.37*** |     -0.43*** |             |\"",
      "[7] \"|Petal.Length |     0.96*** |              |             |\""
    )
  )
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
  mat1 <- datawizard::data_select(mtcars, c("am", "wt", "hp")) |>
    correlation() |>
    as.matrix()
  set.seed(123)
  mat2 <- datawizard::data_select(mtcars, c("am", "wt", "hp")) |>
    datawizard::data_group(am) |>
    correlation() |>
    as.matrix()
  expect_snapshot(list(mat1, mat2))
})
