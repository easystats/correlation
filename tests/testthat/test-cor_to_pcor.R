context("cor_to_pcor")



test_that("pcor_to_cor", {

  if (requireNamespace("ppcor") &
      requireNamespace("Hmisc")) {

    set.seed(333)

    # easycormatrix
    out <- correlation(iris, partial = TRUE, p_adjust="none")
    pcormat <- as.table(out)

    ppcor <- ppcor::pcor(iris[1:4])
    testthat::expect_equal(max(as.matrix(pcormat[2:5]) - as.matrix(ppcor$estimate)), 0, tol = 0.01)

    cormat <- pcor_to_cor(pcormat)
    testthat::expect_equal(max(as.matrix(cormat[2:5]) - as.matrix(cor(iris[1:4]))), 0, tol = 0.01)

    hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
    testthat::expect_equal(mean(as.matrix(cormat[2:5]) - hmisc$r), 0, tol = 0.0001)

    p <- as.matrix(attributes(cormat)$p[2:5])
    testthat::expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tol = 0.001)

    # easycorrelation
    cormat <- as.table(pcor_to_cor(correlation(iris, partial = TRUE)))

    testthat::expect_equal(max(as.matrix(cormat[2:5]) - as.matrix(cor(iris[1:4]))), 0, tol = 0.01)

    hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
    testthat::expect_equal(mean(as.matrix(cormat[2:5]) - hmisc$r), 0, tol = 0.0001)

    p <- as.matrix(attributes(cormat)$p[2:5])
    testthat::expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tol = 0.001)

  }
})



test_that("cor_to_pcor", {

  if (requireNamespace("ppcor")) {

    set.seed(333)

    # easycormatrix
    out <- correlation(iris)
    cormat <- as.table(out)
    pcormat <- cor_to_pcor(cormat)

    ppcor <- ppcor::pcor(iris[1:4])
    testthat::expect_equal(max(as.matrix(pcormat[2:5]) - as.matrix(ppcor$estimate)), 0, tol = 0.01)
    testthat::expect_equal(max(as.matrix(attributes(pcormat)$p[2:5]) - as.matrix(ppcor$p.value)), 0, tol = 0.01)

    # easycorrelation
    pcormat <- as.table(cor_to_pcor(correlation(iris)))

    ppcor <- ppcor::pcor(iris[1:4])
    testthat::expect_equal(max(as.matrix(pcormat[2:5]) - as.matrix(ppcor$estimate)), 0, tol = 0.01)
    testthat::expect_equal(max(as.matrix(attributes(pcormat)$p[2:5]) - as.matrix(ppcor$p.value)), 0, tol = 0.01)

  }
})



test_that("multilevel correlations", {
  # TODO
  set.seed(333)
})




test_that("spcor_to_cor", {

  if (requireNamespace("ppcor")) {

    set.seed(333)

    # easycormatrix
    out <- correlation(iris)
    cormat <- as.table(out)
    spcormat <- cor_to_spcor(cormat, cov = cov(iris[1:4]))

    spcor <- ppcor::spcor(iris[1:4])
    testthat::expect_equal(max(spcormat - as.matrix(spcor$estimate)), 0, tol = 0.01)

  }
})
