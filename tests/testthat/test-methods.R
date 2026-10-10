test_that("as.matrix.correlation", {
  rez <- correlation(mtcars)
  m <- as.matrix(rez)
  expect_equal(dim(m), c(11, 11))
})

test_that("summary.correlation - target column", {
  skip_if_not_or_load_if_installed("ggplot2")
  expect_snapshot(summary(correlation(ggplot2::msleep), target = "t"))
  expect_snapshot(summary(correlation(ggplot2::msleep), target = "df_error"))
  expect_snapshot(summary(correlation(ggplot2::msleep), target = "p"))
  expect_error(summary(
    correlation(ggplot2::msleep),
    target = "not_a_column_name"
  ))
})

test_that("subsetting a correlation matrix keeps its attributes", {
  x <- summary(correlation(iris))
  out <- x[1:2, 1:3]

  expect_s3_class(out, "easycormatrix")
  expect_identical(attr(out, "p"), attr(x, "p")[1:2, 1:3])
  expect_identical(attr(out, "CI_low"), attr(x, "CI_low")[1:2, 1:3])
  expect_identical(attr(out, "n_Obs"), attr(x, "n_Obs")[1:2, 1:3])
  expect_identical(attr(out, "method"), attr(x, "method"))
  expect_identical(attr(out, "p_adjust"), attr(x, "p_adjust"))

  # Stars are recomputed from the subsetted p-values
  formatted <- format(out)
  expect_identical(formatted[[2]], c("0.82***", "-0.37***"))
  expect_match(attr(formatted, "table_footer"), "Holm", fixed = TRUE)

  # Rows and columns are matched by name, in the selected order
  out <- x[c(3, 1), c("Parameter", "Sepal.Width", "Petal.Width")]
  expect_identical(
    attr(out, "p"),
    attr(x, "p")[c(3, 1), c("Parameter", "Sepal.Width", "Petal.Width")]
  )
  out <- x[x$Parameter == "Sepal.Width", ]
  expect_identical(attr(out, "p"), attr(x, "p")[2, ])

  # List-style selection keeps all rows
  out <- x[c("Parameter", "Petal.Length")]
  expect_identical(attr(out, "t"), attr(x, "t")[c("Parameter", "Petal.Length")])
})

test_that("subsetting a correlation matrix without labels behaves as before", {
  x <- summary(correlation(iris))
  expect_identical(x[, 2], c(x$Petal.Width))
  out <- x[, 2:3]
  expect_null(attr(out, "p"))
})

test_that("sorting a correlation matrix keeps its attributes aligned", {
  x <- summary(
    correlation(mtcars[c("mpg", "wt", "hp", "qsec")]),
    redundant = TRUE
  )
  out <- cor_sort(x)
  expect_false(identical(out$Parameter, x$Parameter))

  # Each attribute cell matches the same pair in the unsorted matrix
  for (i in c("p", "CI_low", "n_Obs")) {
    a <- attr(out, i)
    expect_identical(a$Parameter, out$Parameter)
    expect_identical(names(a), names(out))
    ref <- attr(x, i)
    row.names(ref) <- ref$Parameter
    ref <- ref[out$Parameter, names(out)]
    expect_identical(unname(as.list(a)), unname(as.list(ref)))
  }
})
