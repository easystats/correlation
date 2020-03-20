# correlation 0.1.x

## Changes

- Add `plot()`-method for `summary()`.

## Bug fixes

- Fixed issue in `correlation()` for some edge cases when `include_factors = TRUE`.
- Fixed issue in `correlation()` for correlation coefficients with less than four complete pairs of observations (in such cases, `cor_test()` now returns `NA` for the confidence intervals).