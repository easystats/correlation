# select specific variables for correlation -----------------------------

if (requiet("poorman")) {
  test_that("selecting specific variables works", {
    set.seed(123)
    df1 <-
      mtcars %>%
      correlation(
        select = c("cyl", "wt"),
        select2 = c("hp")
      )

    set.seed(123)
    df2 <-
      mtcars %>%
      group_by(am) %>%
      correlation(
        select = c("cyl", "wt"),
        select2 = c("hp")
      )

    set.seed(123)
    df3 <-
      mtcars %>%
      correlation(select = "wt", select2 = "hp")

    set.seed(123)
    df4 <-
      mtcars %>%
      correlation(select = c("wt", "hp"))

    expect_snapshot(list(df1, df2, df3, df4))

    expect_equal(df3$r, df4$r, tolerance = 0.001)
    expect_equal(df3$t, df4$t, tolerance = 0.001)
  })
}
