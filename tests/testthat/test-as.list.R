if (require("poorman", quietly = TRUE) && require("ggplot2", quietly = TRUE)) {
  test_that("as.list", {

    # no groups
    set.seed(123)
    expect_snapshot(as.list(correlation(mtcars)))

    # with groups
    set.seed(123)
    expect_snapshot(as.list(correlation(group_by(msleep, vore), method = "spearman")))

    expect_snapshot(as.list(mtcars %>%
      group_by(am) %>%
      correlation(
        select = c("cyl", "wt"),
        select2 = c("hp"),
        method = "percentage"
      )))
  })
}
