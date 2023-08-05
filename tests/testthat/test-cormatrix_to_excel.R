test_that("cormatrix_to_excel select", {
  .old_wd <- setwd(tempdir())
  expect_snapshot(cormatrix_to_excel(mtcars,
                                     filename = "cormatrix1",
                                     overwrite = TRUE,
                                     p_adjust = "none",
                                     print.mat = TRUE,
                                     select = c("mpg", "cyl", "disp", "hp", "carb")))
  setwd(.old_wd)
})

test_that("cormatrix_to_excel p_adjust", {
  .old_wd <- setwd(tempdir())
  expect_snapshot(cormatrix_to_excel(airquality,
                                     filename = "cormatrix1",
                                     overwrite = FALSE,
                                     p_adjust = "holm",
                                     print.mat = FALSE,
                                     method = "spearman"))
  setwd(.old_wd)
})

