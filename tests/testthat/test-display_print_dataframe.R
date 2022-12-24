requiet("gt")

test_that("display and print method works - markdown", {
  skip_on_cran()

  expect_snapshot(print(correlation(iris)))

  expect_snapshot(display(correlation(iris)))
})

# display and print method works - HTML -----------------------------

test_that("display and print method works - HTML", {
  skip_on_cran()

  expect_snapshot(display(print(correlation(subset(mtcars, select = c("wt", "mpg"))), format = "html")))

  expect_snapshot(print(correlation(subset(mtcars, select = c("wt", "mpg"))), format = "html"))
})
