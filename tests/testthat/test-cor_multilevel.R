test_that("comparison rmcorr", {
  skip_if_not_or_load_if_installed("lme4")
  skip_if_not_or_load_if_installed("rmcorr")
  set.seed(123)
  rez_rmcorr <- rmcorr::rmcorr(
    Species,
    Sepal.Length,
    Sepal.Width,
    dataset = iris
  )

  set.seed(123)
  rez <- cor_test(
    iris[c(1, 2, 5)],
    "Sepal.Length",
    "Sepal.Width",
    partial = TRUE,
    multilevel = TRUE
  )

  expect_equal(rez$r, rez_rmcorr$r, tolerance = 0.001)
  expect_equal(rez$p, rez_rmcorr$p, tolerance = 0.001)
  # expect_equal(rez$df_error, rez_rmcorr$df)
  expect_equal(rez$CI_low, rez_rmcorr$CI[1], tolerance = 0.01)
  expect_equal(rez$CI_high, rez_rmcorr$CI[2], tolerance = 0.01)
})


test_that("Reductio ad absurdum", {
  skip_if_not_or_load_if_installed("lme4")
  cormatrix <- matrix(
    c(
      1.0,
      0.3,
      0.6,
      0.3,
      1.0,
      0.0,
      0.6,
      0.0,
      1.0
    ),
    nrow = 3
  )

  data <- bayestestR::simulate_correlation(n = 500, r = cormatrix)
  # Add factor levels "at random", so the grouping structure should NOT change much
  data$Group <- sample(rep_len(c("A", "B", "C"), length.out = 500))

  rez <- correlation(data)
  expect_equal(max(as.matrix(rez) - cormatrix), 0, tolerance = 0.000001)

  rez <- suppressMessages(correlation(data, multilevel = TRUE, verbose = FALSE))
  expect_equal(max(as.matrix(rez) - cormatrix), 0, tolerance = 0.01)

  rez <- suppressMessages(correlation(
    data,
    multilevel = TRUE,
    partial = TRUE,
    verbose = FALSE
  ))
  expect_equal(
    max(as.matrix(pcor_to_cor(rez)) - cormatrix),
    0,
    tolerance = 0.01
  )
})


test_that("multilevel back-conversion works with select2", {
  skip_if_not_or_load_if_installed("lme4")
  d <- transform(mtcars, gear = factor(gear))

  full <- correlation(
    d,
    select = c("gear", "hp", "wt", "mpg"),
    multilevel = TRUE
  )
  expect_silent({
    rez <- correlation(
      d,
      select = c("gear", "hp", "wt"),
      select2 = "mpg",
      multilevel = TRUE
    )
  })

  expected <- full[full$Parameter2 == "mpg", ]
  expect_identical(rez$Parameter1, c("hp", "wt"))
  expect_identical(rez$Parameter2, c("mpg", "mpg"))
  expect_equal(rez$r, expected$r, tolerance = 1e-10)
  expect_equal(rez$CI_low, expected$CI_low, tolerance = 1e-10)
  expect_equal(rez$CI_high, expected$CI_high, tolerance = 1e-10)
  expect_true(all(abs(rez$r) < 1))
})


test_that("multilevel back-conversion keeps the Method label", {
  skip_if_not_or_load_if_installed("lme4")
  d <- transform(mtcars, gear = factor(gear))

  rez <- correlation(
    d,
    select = c("gear", "hp", "wt", "mpg"),
    multilevel = TRUE,
    method = "spearman"
  )
  expect_identical(unique(rez$Method), "Spearman correlation")

  rez <- correlation(
    d,
    select = c("gear", "hp", "wt", "mpg"),
    multilevel = TRUE
  )
  expect_identical(unique(rez$Method), "Pearson correlation")
})


test_that("multilevel back-conversion works with grouped data", {
  skip_if_not_or_load_if_installed("lme4")
  skip_if_not_or_load_if_installed("dplyr")
  d <- transform(mtcars, gear = factor(gear))

  rez <- correlation(
    dplyr::group_by(d, am),
    select = c("gear", "hp", "wt", "mpg"),
    multilevel = TRUE
  )
  expect_identical(unique(rez$Group), c("0", "1"))

  for (g in c(0, 1)) {
    expected <- correlation(
      d[d$am == g, ],
      select = c("gear", "hp", "wt", "mpg"),
      multilevel = TRUE
    )
    observed <- rez[rez$Group == as.character(g), ]
    expect_equal(observed$r, expected$r, tolerance = 1e-10)
    expect_equal(observed$p, expected$p, tolerance = 1e-10)
  }
})
