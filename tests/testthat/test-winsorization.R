test_that("testing Winsorized correlation", {
  if (requireNamespace("WRS2")) {

    df <- data.frame(x = mtcars$wt, y = mtcars$mpg)

    set.seed(123)
    params1 <- as.data.frame(correlation::correlation(df, winsorize = TRUE))
    params2 <- as.data.frame(correlation::correlation(df, winsorize = 0.3))

    set.seed(123)
    mod1 <- WRS2::wincor(df$x, df$y, tr = 0.1)
    mod2 <- WRS2::wincor(df$x, df$y, tr = 0.3)

    expect_equal(params1$r, mod1$cor, tolerance = 0.001)
    expect_equal(params2$r, mod2$cor, tolerance = 0.001)

    expect_identical(params1$Method[[1]], "Winsorized Pearson correlation")
  }
})
