# correlation 0.5.0

## Changes

- added `winsorize()` function.
- Add "winsorize" argument for Winsorized correlations.

## Bug fixes

- Fix bug in `cor_to_p` that gave slightly different test statistics.


# correlation 0.4.0

## Changes

- Don't error if less than 3 valid observations ([#100](https://github.com/easystats/correlation/issues/100)).
- Add "gaussian" rank method.
- Add "gamma" method.
- Add "hoeffding" method.
- Add "blomqvist" method.

## Bug fixes

- Added `Method` column to Bayesian correlations.
- Fix bug when `robust=TRUE` ([#87](https://github.com/easystats/effectsize/issues/87)).

# correlation 0.3.0

## Changes

## Bug fixes

# correlation 0.2.1

## Changes

- Added confidence intervals CI support for Spearman and Kendall (#80)
- Improved documentation (#45, #63)

## Bug fixes

- Removed CI threshold column from `distance_mahalanobis()`
- Fixed bug (#76)

# correlation 0.2.0

## Changes

- Some changes were made.

## Bug fixes

- Some bugs were fixed.

# correlation 0.1.0

## Changes

- Initial CRAN release.
- Add `plot()`-method for `summary()`.

## Bug fixes

- Fixed issue in `correlation()` for some edge cases when `include_factors = TRUE`.
- Fixed issue in `correlation()` for correlation coefficients with less than four complete pairs of observations (in such cases, `cor_test()` now returns `NA` for the confidence intervals).
