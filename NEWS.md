# correlation 0.8.2

- Maintenance release for *datawizard* package update.

# correlation 0.8.1

- Maintenance release for *datawizard* package update.

# correlation 0.8.0

## Breaking Changes

- `robust` argument, which was deprecated in favour of `ranktransform` in
  `0.6.1` release, has now been removed.

# correlation 0.7.1

## Bug Fixes

- Bug fix in `plot()` methods

# correlation 0.7.0

## Breaking Changes

- Removes `winsorize()` function, which now lives in `datawizard` package.

## New Features

- New `cor_smooth()` function for smoothing non-positive definite matrices.

## Bug Fixes

- When `data2` was specified `correlation()` was over-correcting for all of the
  combinations of variables in the full x and y tables, rather than in just the
  ones specified (#195).

## Minor Changes

- `correlation()` gains a new argument `rename` to rename variables.

- `simualte_simpson()` function is now re-exported from `bayestestR` package.

- `plot()` for `"easycor_test"` objects now produces an annotated scatter plot.

# correlation 0.6.1

## Breaking Changes

- `simualte_simpson()`: The groups are now named after the pattern `"G_"` (can
  be altered with the `group_prefix` argument).

- `robust` argument deprecated in favour of `ranktransform`.

## New Features

- `correlation` gains two new arguments: `select` and `select2` to select
  specific variables from dataframes to compare (#146).

- `as.matrix` method works for grouped correlations (#148).

- New `as.list` method returns a list of various matrices related to correlation
  analysis (correlation, number of observations, *p*-values, etc.).

## Bug Fixes

- The `0.6.0` release introduced a bug in Winsorized Pearson correlation where
  the missing values were removed from the entire data, instead for each pair
  (#151). This is now fixed.

# correlation 0.6.0

## New Features

- Added `verbose` arguments to some functions, to toggle warnings on/off.

- `cor_test()` (and hence, `correlation()`) now default the `winsorize` argument
  to `.1` when it's set to `TRUE`.

- The `Method` column in output dataframe is now more explicit about the
  correlation method used.

## Bug Fixes

- Winsorization doesn't fail when `NA`s are present (#130).

## Minor Changes

- Fixed CRAN check issues due to changes in dependent packages.

# correlation 0.5.0

## Changes

- Added `winsorize()` function.

- Added `winsorize` argument for Winsorized correlations.

- Added `method = "somers"` to `correlation()`, to compute Somers's Dxy rank
  correlation for binary outcomes.

- New function `display()`, to print output into different formats. Currently,
  only markdown is supported. `print_md()` is an alias for `display(format =
  "markdown")`.

## Bug fixes

- Fix bug in `cor_to_p()` that gave slightly different test statistics.

# correlation 0.4.0

## Changes

- Don't error if less than 3 valid observations
  ([#100](https://github.com/easystats/correlation/issues/100)).

- Add "gaussian" rank method.

- Add "gamma" method.

- Add "hoeffding" method.

- Add "blomqvist" method.

## Bug fixes

- Added `Method` column to Bayesian correlations.

- Fix bug when `robust=TRUE`
  ([#87](https://github.com/easystats/effectsize/issues/87)).

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

- Fixed issue in `correlation()` for some edge cases when `include_factors =
  TRUE`.

- Fixed issue in `correlation()` for correlation coefficients with less than
  four complete pairs of observations (in such cases, `cor_test()` now returns
  `NA` for the confidence intervals).

