test_that("testing Winsorized correlation", {
  if (requireNamespace("WRS2")) {
    df <- data.frame(x = mtcars$wt, y = mtcars$mpg)

    expect_equal(correlation(df, winsorize = 1.5)$r, correlation(df)$r, tolerance = 0.01)
    expect_equal(correlation(df, winsorize = 1.5, verbose = FALSE)$r, correlation(df)$r, tolerance = 0.01)

    set.seed(123)
    params1 <- as.data.frame(correlation(df, winsorize = TRUE))
    params2 <- as.data.frame(correlation(df, winsorize = 0.3))
    params3 <- as.data.frame(correlation(df, winsorize = TRUE, bayesian = TRUE))
    params4 <- as.data.frame(correlation(df, winsorize = 0.3, bayesian = TRUE, bayesian_prior = 0.8))

    set.seed(123)
    mod1 <- WRS2::wincor(df$x, df$y, tr = 0.1)
    mod2 <- WRS2::wincor(df$x, df$y, tr = 0.3)

    expect_equal(params1$r, mod1$cor, tolerance = 0.001)
    expect_equal(params2$r, mod2$cor, tolerance = 0.001)

    expect_equal(params1$t, mod1$test, tolerance = 0.001)
    expect_equal(params2$t, mod2$test, tolerance = 0.001)

    expect_identical(params1$Method[[1]], "Winsorized Pearson correlation")

    expect_equal(params3$rho, -0.8523543, tolerance = 0.001)
    expect_equal(params4$rho, -0.8242469, tolerance = 0.001)

    # if (packageVersion("insight") >= "0.12.0.1") {
    #   expect_equal(params3$Prior_Location, 3, tolerance = 0.1)
    #   expect_equal(params4$Prior_Location, -1.25, tolerance = 0.1)
    # }
  }
})

test_that("testing Winsorization of factors", {
  expect_equal(winsorize(as.factor(mtcars$am)), as.factor(mtcars$am))
})
