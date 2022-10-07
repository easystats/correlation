test_that("renaming columns", {
  # should warn the user
  expect_snapshot(correlation(anscombe,
    select = c("x1", "x2"),
    rename = c("var1")
  ))

  expect_snapshot(correlation(anscombe,
    select = c("x1", "x2"),
    rename = c("var1", "var2")
  ))

  expect_snapshot(correlation(anscombe,
    select = c("x1", "x2"),
    select2 = c("y1", "y2"),
    rename = c("var1", "var2")
  ))
})
