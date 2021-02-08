if (require("gt") && packageVersion("insight") >= "0.12.0.1") {

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

    expect_output(print(summary(correlation(iris)), format = "html"))

    # don't run this
    # waldo can't handle this
    # Error: Internal Error: reached theoretically unreachable branch 2, contact maintainer.
    # expect_snapshot(display(summary(correlation(iris)), format = "html"), cran = FALSE)

    expect_snapshot(print(summary(correlation(iris)), format = "html"), cran = FALSE)
  })
}
