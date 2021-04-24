if (require("dplyr", quietly = TRUE) && require("ggplot2", quietly = TRUE)) {
  test_that("as.list", {

    # default target_col
    set.seed(123)
    expect_snapshot(summary(correlation(mtcars)))

    # custom target_col
    set.seed(123)
    expect_snapshot(summary(correlation(mtcars), target_col = "p"))

    # error -- invalid target_col
    set.seed(123)
    expect_error(summary(correlation(mtcars), target_col = "not_a_column"))
  })
}
