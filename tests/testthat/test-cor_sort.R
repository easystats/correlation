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
  # Square
  rez1 <- correlation::correlation(mtcars)
  rez1sort <- cor_sort(rez1)
  expect_false(all(rez1$Parameter1 == rez1sort$Parameter1))

  # Non-square
  rez2 <- correlation::correlation(mtcars[names(mtcars)[1:5]], mtcars[names(mtcars)[6:11]])
  rez2sort <- cor_sort(rez2)
  expect_false(all(rez2$Parameter1 == rez2sort$Parameter1))

  # summary(correlation()) --------------------------------------------------
  # Square
  # rez1sum <- summary(rez1)  # TODO: doesn't work with non-redundant
  # rez1sumsort <- cor_sort(rez1sum)

  rez1sum <- summary(rez1, redundant=TRUE)
  rez1sumsort <- cor_sort(rez1sum)
  expect_false(all(rownames(rez1sumsort) == rownames(rez1sum)))

  # Non-square
  rez2sum <- summary(rez2)
  rez2sumsort <- cor_sort(rez2sum)
  expect_false(all(rownames(rez2sumsort) == rownames(rez2sum)))

  # as.matrix(correlation()) ------------------------------------------------
  # TODO.
  m1 <- as.matrix(rez1)
  # m1sort <- as.matrix(rez1sort)
})
