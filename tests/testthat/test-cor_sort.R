test_that("cor_sort", {
  r <- cor(mtcars)
  expect_equal(as.numeric(diag(r)), rep(1, ncol(mtcars)))

  r1 <- cor_sort(r)
  expect_equal(as.numeric(diag(r)), rep(1, ncol(mtcars)))

  r2 <- cor(mtcars[names(mtcars)[1:5]], mtcars[names(mtcars)[6:11]])
  expect_equal(rownames(r2), names(mtcars)[1:5])
  r3 <- cor_sort(r2)
  expect_equal(all(rownames(r3) == names(mtcars)[1:5]), FALSE)
})