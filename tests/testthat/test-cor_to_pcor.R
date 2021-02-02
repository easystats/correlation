
test_that("pcor_to_cor", {
  if (requireNamespace("ppcor") &
    requireNamespace("Hmisc")) {
    set.seed(333)

    # easycormatrix
    out <- correlation(iris, partial = TRUE, p_adjust = "none")
    pcormat <- summary(out, redundant = TRUE)

    ppcor <- ppcor::pcor(iris[1:4])
    expect_equal(max(as.matrix(pcormat[2:5]) - as.matrix(ppcor$estimate)), 0, tolerance = 0.01)

    cormat <- pcor_to_cor(pcormat)
    expect_equal(max(as.matrix(cormat[2:5]) - as.matrix(cor(iris[1:4]))), 0, tolerance = 0.01)

    hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
    expect_equal(mean(as.matrix(cormat[2:5]) - hmisc$r), 0, tolerance = 0.0001)

    p <- as.matrix(attributes(cormat)$p[2:5])
    expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tolerance = 0.001)

    # easycorrelation
    cormat <- summary(pcor_to_cor(correlation(iris, partial = TRUE)), redundant = TRUE)

    expect_equal(max(as.matrix(cormat[2:5]) - as.matrix(cor(iris[1:4]))), 0, tolerance = 0.01)

    hmisc <- Hmisc::rcorr(as.matrix(iris[1:4]), type = c("pearson"))
    expect_equal(mean(as.matrix(cormat[2:5]) - hmisc$r), 0, tolerance = 0.0001)

    p <- as.matrix(attributes(cormat)$p[2:5])
    expect_equal(mean(p - hmisc$P, na.rm = TRUE), 0, tolerance = 0.001)
  }
})



test_that("cor_to_pcor", {
  if (requireNamespace("ppcor")) {
    set.seed(333)

    # easycormatrix
    out <- correlation(iris)
    cormat <- summary(out, redundant = TRUE)
    pcormat <- cor_to_pcor(cormat)

    ppcor <- ppcor::pcor(iris[1:4])
    expect_equal(max(as.matrix(pcormat[2:5]) - as.matrix(ppcor$estimate)), 0, tolerance = 0.01)
    expect_equal(max(as.matrix(attributes(pcormat)$p[2:5]) - as.matrix(ppcor$p.value)), 0, tolerance = 0.01)

    # easycorrelation
    pcormat <- summary(cor_to_pcor(correlation(iris)), redundant = TRUE)

    ppcor <- ppcor::pcor(iris[1:4])
    expect_equal(max(as.matrix(pcormat[2:5]) - as.matrix(ppcor$estimate)), 0, tolerance = 0.01)
    expect_equal(max(as.matrix(attributes(pcormat)$p[2:5]) - as.matrix(ppcor$p.value)), 0, tolerance = 0.01)
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
    cormat <- summary(out, redundant = TRUE)
    spcormat <- cor_to_spcor(cormat, cov = cov(iris[1:4]))

    spcor <- ppcor::spcor(iris[1:4])
    expect_equal(max(spcormat - as.matrix(spcor$estimate)), 0, tolerance = 0.01)
  }
})
