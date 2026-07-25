test_that("cor_diff", {
  expect_equal(
    cor_diff(iris, "Sepal.Length", "Sepal.Width", "Sepal.Length", "Petal.Width")$t,
    -10,
    tolerance = 0.001
  )
})
