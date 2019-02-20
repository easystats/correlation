context("correlation")



test_that("correlation frequentist", {
  library(dplyr)

  out <- correlation(iris)
  testthat::expect_equal(nrow(out), 16)

  out <- correlation(iris, bayesian=TRUE)
  testthat::expect_equal(nrow(out), 16)

  out <- correlation(iris, partial = TRUE)
  testthat::expect_equal(nrow(out), 16)

  out <- correlation(iris, partial = "semi")
  testthat::expect_equal(nrow(out), 16)
  testthat::expect_equal(length(as.list(out)), 10)

  out <- iris %>%
    select(starts_with("Sepal")) %>%
    correlation(iris %>%
      select(starts_with("Petal")))
  testthat::expect_equal(nrow(out), 4)

  out <- iris %>%
    group_by(Species) %>%
    correlation()
  testthat::expect_equal(nrow(out), 48)

  out <- iris %>%
    group_by(Species) %>%
    select(Species, starts_with("Petal")) %>%
    correlation(iris %>%
      group_by(Species) %>%
      select(Species, starts_with("Sepal")))
  testthat::expect_equal(nrow(out), 12)

  testthat::expect_error(iris %>%
    group_by(Species) %>%
    correlation(iris))
})
