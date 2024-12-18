test_that("cor_sort", {

  # Basic -------------------------------------------------------------------

  # Square
  r1 <- cor(mtcars)
  expect_equal(as.numeric(diag(r1)), rep(1, ncol(mtcars)))
  # heatmap(r1, Rowv = NA, Colv = NA)  # visualize

  r1sort <- cor_sort(r1)
  expect_equal(as.numeric(diag(r1sort)), rep(1, ncol(mtcars)))
  # heatmap(r1sort, Rowv = NA, Colv = NA)  # visualize

  # Non-square
  r2 <- cor(mtcars[names(mtcars)[1:5]], mtcars[names(mtcars)[6:11]])
  expect_equal(rownames(r2), names(mtcars)[1:5])
  # heatmap(r2, Rowv = NA, Colv = NA)  # visualize

  r2sort <- cor_sort(r2)
  expect_equal(all(rownames(r2sort) == names(mtcars)[1:5]), FALSE)
  # heatmap(r2sort, Rowv = NA, Colv = NA)  # visualize

  # correlation() -----------------------------------------------------------
  # TODO.
  rez1 <- correlation::correlation(mtcars)
  # rez1sort <- cor_sort(rez1)

  # summary(correlation()) --------------------------------------------------
  # TODO.

  # as.matrix(correlation()) ------------------------------------------------
  # TODO.
  m1 <- as.matrix(rez1)
  expect_equal(rownames(r1), rownames(m1))

  # m1sort <- as.matrix(rez1sort)
})
