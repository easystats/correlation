test_that("cormatrix_to_excel select", {
  skip_if_not_or_load_if_installed("openxlsx2")
  expect_snapshot(suppressWarnings(cormatrix_to_excel(
    mtcars,
    filename = "cormatrix1",
    overwrite = TRUE,
    p_adjust = "none",
    print.mat = TRUE,
    select = c("mpg", "cyl", "disp", "hp", "carb"),
    verbose = FALSE
  )))
  unlink("cormatrix1.xlsx")
})

test_that("cormatrix_to_excel p_adjust", {
  skip_if_not_or_load_if_installed("openxlsx2")
  expect_snapshot(suppressWarnings(cormatrix_to_excel(
    airquality,
    filename = "cormatrix1",
    overwrite = FALSE,
    p_adjust = "holm",
    print.mat = FALSE,
    method = "spearman",
    verbose = FALSE
  )))
  unlink("cormatrix1.xlsx")
})
