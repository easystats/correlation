
if (requiet("poorman")) {
  df <- subset(mtcars, select = c("am", "mpg", "wt")) %>% group_by(am)

  expect_error(
    correlation(
      subset(df, select = c("am", "mpg")),
      subset(df, select = c("wt"))
    )
  )

  corr_df <- correlation(
    subset(df, select = c("am", "mpg")),
    subset(df, select = c("am", "wt"))
  )

  expect_equal(corr_df$r, c(-0.7676554, -0.9089148), tolerance = 0.001)
}
