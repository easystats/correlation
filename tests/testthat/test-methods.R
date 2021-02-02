test_that("as.matrix.correlation", {
  rez <- correlation::correlation(mtcars)
  m <- as.matrix(rez)
  expect_equal(dim(m), c(11, 11))
})
