skip_on_cran()
skip_if_not_installed("discovr")

test_that("discovr-chap_7.4", {
  data(exam_anxiety, package = "discovr")
  exam_tib <- discovr::exam_anxiety
  out <- correlation(
    exam_tib,
    select = c("exam_grade", "revise"),
    rename = c("Exam grade (%)", "Time revising")
  )
  expect_identical(
    capture.output(out),
    c(
      "# Correlation Matrix (pearson-method)",
      "",
      "Parameter1     |    Parameter2 |    r |       95% CI | t(101) |         p",
      "-------------------------------------------------------------------------",
      "Exam grade (%) | Time revising | 0.40 | [0.22, 0.55] |   4.34 | < .001***",
      "",
      "p-value adjustment method: Holm (1979)",
      "Observations: 103"
    )
  )

  out <- correlation(
    exam_tib,
    select = c("exam_grade", "revise", "anxiety")
  )
  expect_identical(
    capture.output(out),
    c(
      "# Correlation Matrix (pearson-method)",
      "",
      "Parameter1 | Parameter2 |     r |         95% CI | t(101) |         p",
      "---------------------------------------------------------------------",
      "exam_grade |     revise |  0.40 | [ 0.22,  0.55] |   4.34 | < .001***",
      "exam_grade |    anxiety | -0.44 | [-0.58, -0.27] |  -4.94 | < .001***",
      "revise     |    anxiety | -0.71 | [-0.79, -0.60] | -10.11 | < .001***",
      "",
      "p-value adjustment method: Holm (1979)",
      "Observations: 103"
    )
  )

  out <- summary(correlation(
    exam_tib,
    select = c("exam_grade", "revise", "anxiety")
  ))
  expect_identical(
    capture.output(out),
    c(
      "# Correlation Matrix (pearson-method)",
      "",
      "Parameter  |  anxiety |  revise",
      "-------------------------------",
      "exam_grade | -0.44*** | 0.40***",
      "revise     | -0.71*** |        ",
      "",
      "p-value adjustment method: Holm (1979)"
    )
  )

  out <- correlation(
    exam_tib,
    select = c("exam_grade", "revise", "anxiety"),
    winsorize = 0.2
  )
  expect_identical(
    capture.output(out),
    c(
      "# Correlation Matrix (pearson-method)",
      "",
      "Parameter1 | Parameter2 |     r |         95% CI | t(101) |         p",
      "---------------------------------------------------------------------",
      "exam_grade |     revise |  0.31 | [ 0.12,  0.47] |   3.26 | 0.002**  ",
      "exam_grade |    anxiety | -0.39 | [-0.54, -0.21] |  -4.27 | < .001***",
      "revise     |    anxiety | -0.60 | [-0.71, -0.46] |  -7.57 | < .001***",
      "",
      "p-value adjustment method: Holm (1979)",
      "Observations: 103"
    )
  )

  out <- correlation(
    exam_tib,
    select = c("exam_grade", "revise", "anxiety"),
    method = "percentage",
    beta = 0.2
  )
  expect_identical(
    capture.output(out),
    c(
      "# Correlation Matrix (percentage-method)",
      "",
      "Parameter1 | Parameter2 |     r |         95% CI | t(101) |         p",
      "---------------------------------------------------------------------",
      "exam_grade |     revise |  0.34 | [ 0.15,  0.50] |   3.60 | < .001***",
      "exam_grade |    anxiety | -0.40 | [-0.55, -0.23] |  -4.41 | < .001***",
      "revise     |    anxiety | -0.61 | [-0.72, -0.47] |  -7.66 | < .001***",
      "",
      "p-value adjustment method: Holm (1979)",
      "Observations: 103"
    )
  )

  out <- correlation(
    discovr::biggest_liar,
    select = c("position", "creativity"),
    method = "spearman"
  )
  expect_identical(
    capture.output(out),
    c(
      "# Correlation Matrix (spearman-method)",
      "",
      "Parameter1 | Parameter2 |   rho |         95% CI |        S |       p",
      "---------------------------------------------------------------------",
      "position   | creativity | -0.38 | [-0.57, -0.14] | 72123.51 | 0.002**",
      "",
      "p-value adjustment method: Holm (1979)",
      "Observations: 68"
    )
  )

  out <- correlation(
    discovr::biggest_liar,
    select = c("position", "creativity"),
    method = "kendall"
  )
  expect_identical(
    capture.output(out),
    c(
      "# Correlation Matrix (kendall-method)",
      "",
      "Parameter1 | Parameter2 |   tau |         95% CI |     z |       p",
      "------------------------------------------------------------------",
      "position   | creativity | -0.30 | [-0.44, -0.15] | -3.24 | 0.001**",
      "",
      "p-value adjustment method: Holm (1979)",
      "Observations: 68"
    )
  )
})
