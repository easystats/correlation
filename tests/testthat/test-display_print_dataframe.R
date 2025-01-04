test_that("display and print method works - markdown", {
  skip_on_cran()
  out <- display(correlation(iris))
  expect_identical(
    out[1:7],
    c(
      "Table: Correlation Matrix (pearson-method)",
      "",
      "|Parameter1   |   Parameter2 |     r |         95% CI | t(148) |         p |",
      "|:------------|:------------:|:-----:|:--------------:|:------:|:---------:|",
      "|Sepal.Length |  Sepal.Width | -0.12 |  (-0.27, 0.04) |  -1.44 | 0.152     |",
      "|Sepal.Length | Petal.Length |  0.87 |   (0.83, 0.91) |  21.65 | < .001*** |",
      "|Sepal.Length |  Petal.Width |  0.82 |   (0.76, 0.86) |  17.30 | < .001*** |"
    )
  )
})

# display and print method works - HTML -----------------------------

test_that("display and print method works - HTML", {
  skip_on_cran()
  skip_if(getRversion() < "4.0.0")
  skip_if_not_or_load_if_installed("gt")

  expect_s3_class(display(correlation(subset(mtcars, select = c("wt", "mpg"))), format = "html"), "gt_tbl")
  expect_s3_class(print_html(correlation(subset(mtcars, select = c("wt", "mpg")))), "gt_tbl")
})
