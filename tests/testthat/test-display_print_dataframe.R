# display and print method works - markdown -----------------------------

if (require("gt") && packageVersion("insight") >= "0.12.0.1") {
  test_that("display and print method works - markdown", {
    skip_on_cran()


    expect_equal(
      display(correlation(iris)),
      structure(c(
        "Table: Correlation Matrix (pearson-method)", "",
        "|Parameter1   |   Parameter2 |     r |         95% CI | t(148) |         p |",
        "|:------------|:------------:|:-----:|:--------------:|:------:|:---------:|",
        "|Sepal.Length |  Sepal.Width | -0.12 |  (-0.27, 0.04) |  -1.44 | 0.152     |",
        "|Sepal.Length | Petal.Length |  0.87 |   (0.83, 0.91) |  21.65 | < .001*** |",
        "|Sepal.Length |  Petal.Width |  0.82 |   (0.76, 0.86) |  17.30 | < .001*** |",
        "|Sepal.Width  | Petal.Length | -0.43 | (-0.55, -0.29) |  -5.77 | < .001*** |",
        "|Sepal.Width  |  Petal.Width | -0.37 | (-0.50, -0.22) |  -4.79 | < .001*** |",
        "|Petal.Length |  Petal.Width |  0.96 |   (0.95, 0.97) |  43.39 | < .001*** |",
        "p-value adjustment method: Holm (1979)", "Observations: 150"
      ), format = "pipe", class = c("knitr_kable", "character"))
    )

    expect_output(print(correlation(iris)))

    expect_snapshot(display(correlation(iris)), cran = FALSE)
  })

  # display and print method works - HTML -----------------------------

  test_that("display and print method works - HTML", {
    skip_on_cran()

    expect_output(display(print(correlation(subset(mtcars, select = c("wt", "mpg"))), format = "html")))

    expect_snapshot(print(correlation(subset(mtcars, select = c("wt", "mpg"))), format = "html"),
      cran = FALSE
    )
  })
}
