if (require("testthat") && require("gt") && require("dplyr")) {

  # display and print method works - markdown -----------------------------

  test_that("display and print method works - markdown", {
    skip_on_cran()

    expect_equal(
      display(summary(correlation(iris))),
      structure(c(
        "Table: Correlation Matrix (pearson-method)", "",
        "|Parameter    | Petal.Width | Petal.Length | Sepal.Width |",
        "|:------------|:-----------:|:------------:|:-----------:|",
        "|Sepal.Length |     0.82*** |      0.87*** |       -0.12 |",
        "|Sepal.Width  |    -0.37*** |     -0.43*** |             |",
        "|Petal.Length |     0.96*** |              |             |",
        "p-value adjustment method: Holm (1979)"
      ), format = "pipe", class = c(
        "knitr_kable",
        "character"
      ))
    )

    expect_output(print(summary(correlation(iris))))

    expect_snapshot(print(summary(correlation(iris))), cran = FALSE)
  })



  # display and print method works - html -----------------------------

  test_that("display and print method works - html", {
    skip_on_cran()

    # to be run when  `diffobj` version  > 0.3.3.9000 is on CRAN
    # expect_snapshot(display(summary(correlation(iris)), format = "html"), cran = FALSE)

    expect_snapshot(print(summary(correlation(iris)), format = "html"), cran = FALSE)
  })

  test_that("as.matrix works", {
    set.seed(123)
    mat1 <- dplyr::select(mtcars, am, wt, hp) %>%
      correlation() %>%
      as.matrix()

    set.seed(123)
    mat2 <- dplyr::select(mtcars, am, wt, hp) %>%
      group_by(am) %>%
      correlation() %>%
      as.matrix()

    expect_snapshot(list(mat1, mat2))
  })
}
