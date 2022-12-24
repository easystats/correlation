if (requiet("gt") && requiet("poorman")) {
  # display and print method works - markdown -----------------------------

  test_that("display and print method works - markdown", {
    skip_on_cran()

    expect_snapshot(display(summary(correlation(iris))))
    expect_snapshot(print(summary(correlation(iris))))
  })



  # display and print method works - html -----------------------------

  test_that("display and print method works - html", {
    skip_on_cran()

    expect_snapshot(print(summary(correlation(iris)), format = "html"))
  })

  test_that("as.matrix works", {
    set.seed(123)
    mat1 <- select(mtcars, am, wt, hp) %>%
      correlation() %>%
      as.matrix()

    set.seed(123)
    mat2 <- select(mtcars, am, wt, hp) %>%
      group_by(am) %>%
      correlation() %>%
      as.matrix()

    expect_snapshot(list(mat1, mat2))
  })
}
