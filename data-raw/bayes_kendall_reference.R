# Reference table for the Bayesian Kendall's tau tests -------------------------
#
# Prints the reference values that tests/testthat/test-cor_test_bayes_kendall.R
# embeds: for every cell of n in {10, 20, 50} x observed tau-b in {-0.3, 0, 0.2,
# 0.5} x prior scale in {1/3, 1}, the Bayes factor BF10, the posterior median,
# and the equal-tailed 95% interval from two independent implementations of
# van Doorn, Ly, Marsman & Wagenmakers (2018), "Bayesian Inference for Kendall's
# Rank Correlation Coefficient", The American Statistician 72(4), 303-308,
# doi:10.1080/00031305.2016.1264998:
#
#   1. The paper's own OSF code, R_Code_KendallBayesFactor.R (https://osf.io/b9qhj/,
#      file download https://osf.io/download/bg4vw/): bfCorrieKernelKendallTau()
#      and credibleIntervalKendallTau().
#   2. Alexander Ly's bstats package, R/kendall.R at commit 42d34c1
#      (https://github.com/AlexanderLyNL/bstats/tree/42d34c18df08d233825bae34fdc0dfa0cd70ce8c/R):
#      computeKendallBCor(). The whole R/ directory is needed because kendall.R
#      calls helpers in 0helpers.R; bstats also needs the purrr package.
#
# Both sources are kept on the gitignored shelf cairn/references/sources/vandoorn2018/
# (see cairn/references/vandoorn2018.md). To regenerate the table, download the
# files to the paths below (or set the two variables) and run:
#
#   Rscript data-raw/bayes_kendall_reference.R
#
# The prior scale is the "kappa" argument of both sources; the stretched-beta
# parameter is alpha = 1/kappa, so scale 1/3 is alpha = 3 (the package's
# "medium" prior) and scale 1 is alpha = 1 (the paper's default).

osf_file <- Sys.getenv(
  "VANDOORN_OSF",
  "cairn/references/sources/vandoorn2018/R_Code_KendallBayesFactor.R"
)
bstats_dir <- Sys.getenv(
  "VANDOORN_BSTATS",
  "cairn/references/sources/vandoorn2018/bstats_R_42d34c1"
)

# Each source in its own environment: both define priorTau() and posteriorTau()
# with different signatures.
osf <- new.env()
grDevices::pdf(NULL) # the OSF script ends by plotting; keep that off disk
sys.source(osf_file, envir = osf)
grDevices::dev.off()
bstats <- new.env()
for (f in list.files(bstats_dir, pattern = "\\.R$", full.names = TRUE)) {
  sys.source(f, envir = bstats)
}

grid <- expand.grid(
  n = c(10, 20, 50),
  tau = c(-0.3, 0, 0.2, 0.5),
  scale = c(1 / 3, 1)
)
grid <- grid[order(grid$scale, grid$n, grid$tau), ]
rows <- lapply(seq_len(nrow(grid)), function(i) {
  n <- grid$n[i]
  tau <- grid$tau[i]
  scale <- grid$scale[i]
  o_bf <- osf$bfCorrieKernelKendallTau(tau = tau, n = n, kappa = scale)$bf10
  o_ci <- osf$credibleIntervalKendallTau(kentau = tau, n = n, kappa = scale)
  b <- bstats$computeKendallBCor(n = n, tauObs = tau, kappa = scale)$two.sided
  data.frame(
    n = n,
    tau = tau,
    scale = scale,
    osf_bf = o_bf,
    osf_median = o_ci$median,
    osf_low = o_ci$lowerCI,
    osf_high = o_ci$upperCI,
    bstats_bf = b$bf,
    bstats_median = b$posteriorMedian,
    bstats_low = b$lowerCi,
    bstats_high = b$upperCi
  )
})
ref <- do.call(rbind, rows)

# Print as R code ready to paste into the test file.
cat("bayes_kendall_reference <- data.frame(\n")
for (col in names(ref)) {
  vals <- if (col == "scale") {
    ifelse(ref[[col]] == 1, "1", "1 / 3")
  } else {
    formatC(ref[[col]], digits = 6, format = "g")
  }
  cat(
    "  ",
    col,
    " = c(",
    paste(vals, collapse = ", "),
    ")",
    if (col != names(ref)[length(ref)]) "," else "",
    "\n",
    sep = ""
  )
}
cat(")\n")
