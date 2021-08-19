library(testthat)
library(correlation)

is_dev_version <- length(strsplit(packageDescription("correlation")$Version, "\\.")[[1]]) > 3
if (is_dev_version) {
  Sys.setenv("RunAllcorrelationTests" = "yes")
} else {
  Sys.setenv("RunAllcorrelationTests" = "no")
}

test_check("correlation")
