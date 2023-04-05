test_that("cor_to_p", {
  expect_message(df <- cor_to_p(-0.1175698, n = 150, method = "kendall"))
  expect_equal(
    df,
    list(p = 0.0327638207025712, statistic = -2.1349655930582),
    tolerance = 0.001
  )
})
