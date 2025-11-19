# Changelog

## correlation 0.8.9

### Bug Fixes

- [`cormatrix_to_excel()`](https://easystats.github.io/correlation/reference/cormatrix_to_excel.md)
  now works correctly with openxlsx2 v1.16+. Fixed conditional
  formatting rule ordering to accommodate openxlsx2’s new waterfall
  strategy
  ([\#361](https://github.com/easystats/correlation/issues/361)).

## correlation 0.8.8

CRAN release: 2025-07-08

- [`correlation()`](https://easystats.github.io/correlation/reference/correlation.md)
  gains a `missing=` argument, similar to `stats::cor(use=)`, for
  controlling how missing data is handled.

- [`correlation()`](https://easystats.github.io/correlation/reference/correlation.md)
  converts numeric input variables automatically into factors when
  `method = "polychoric"`.

## correlation 0.8.7

CRAN release: 2025-03-03

- The [`format()`](https://rdrr.io/r/base/format.html) method for
  objects of class `easycormatrix` gets a `zap_small` argument, to round
  very small numbers.

- [`cor_sort()`](https://easystats.github.io/correlation/reference/cor_sort.md)
  can now deal with non-square matrices.

- Updated required R version to \>= 4.1.0 (released May 2021) to pass
  CRAN checks on documentation

## correlation 0.8.6

CRAN release: 2024-10-26

- Fix CRAN check issues.

## correlation 0.8.5

CRAN release: 2024-06-16

- New
  [`cormatrix_to_excel()`](https://easystats.github.io/correlation/reference/cormatrix_to_excel.md)
  function for exporting correlation matrices to Excel with color
  formatting.
- This release changes the licensing model of
  [correlation](https://easystats.github.io/correlation/) to an MIT
  license.

## correlation 0.8.4

CRAN release: 2023-04-06

- Minor improvements and code revisions due to changes in other
  packages.

- Default color scheme for correlation matrices switched to use red for
  negative values and blue for positive values.

## correlation 0.8.3

CRAN release: 2022-10-08

### Breaking Changes

- [`distance_mahalanobis()`](https://easystats.github.io/correlation/reference/correlation-deprecated.md)
  is deprecated. Use
  `performance::check_outliers(method = "mahalanobis_robust")` instead.

- The minimum needed R version has been bumped to `3.6`.

### Minor Changes

- Fixes breakages caused by updates to *parameters* package
  ([\#269](https://github.com/easystats/correlation/issues/269)).

- The visualization recipe (plots) for redundant correlation matrices
  was improved, so self-correlations will no longer be labelled and get
  a neutral color.

- The [`print()`](https://rdrr.io/r/base/print.html) method redundant
  correlation matrices no longer shows the diagonal with
  self-correlations.

## correlation 0.8.2

CRAN release: 2022-08-09

- Maintenance release for *datawizard* package update.

## correlation 0.8.1

CRAN release: 2022-05-20

- Maintenance release for *datawizard* package update.

## correlation 0.8.0

CRAN release: 2022-02-14

### Breaking Changes

- `robust` argument, which was deprecated in favour of `ranktransform`
  in `0.6.1` release, has now been removed.

## correlation 0.7.1

CRAN release: 2021-10-06

### Bug Fixes

- Bug fix in [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  methods

## correlation 0.7.0

CRAN release: 2021-09-05

### Breaking Changes

- Removes
  [`winsorize()`](https://easystats.github.io/datawizard/reference/winsorize.html)
  function, which now lives in `datawizard` package.

### New Features

- New
  [`cor_smooth()`](https://easystats.github.io/correlation/reference/cor_smooth.md)
  function for smoothing non-positive definite matrices.

### Bug Fixes

- When `data2` was specified
  [`correlation()`](https://easystats.github.io/correlation/reference/correlation.md)
  was over-correcting for all of the combinations of variables in the
  full x and y tables, rather than in just the ones specified
  ([\#195](https://github.com/easystats/correlation/issues/195)).

### Minor Changes

- [`correlation()`](https://easystats.github.io/correlation/reference/correlation.md)
  gains a new argument `rename` to rename variables.

- `simualte_simpson()` function is now re-exported from `bayestestR`
  package.

- [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  `"easycor_test"` objects now produces an annotated scatter plot.

## correlation 0.6.1

CRAN release: 2021-04-09

### Breaking Changes

- `simualte_simpson()`: The groups are now named after the pattern
  `"G_"` (can be altered with the `group_prefix` argument).

- `robust` argument deprecated in favour of `ranktransform`.

### New Features

- `correlation` gains two new arguments: `select` and `select2` to
  select specific variables from dataframes to compare
  ([\#146](https://github.com/easystats/correlation/issues/146)).

- `as.matrix` method works for grouped correlations
  ([\#148](https://github.com/easystats/correlation/issues/148)).

- New `as.list` method returns a list of various matrices related to
  correlation analysis (correlation, number of observations, *p*-values,
  etc.).

### Bug Fixes

- The `0.6.0` release introduced a bug in Winsorized Pearson correlation
  where the missing values were removed from the entire data, instead
  for each pair
  ([\#151](https://github.com/easystats/correlation/issues/151)). This
  is now fixed.

## correlation 0.6.0

CRAN release: 2021-02-12

### New Features

- Added `verbose` arguments to some functions, to toggle warnings
  on/off.

- [`cor_test()`](https://easystats.github.io/correlation/reference/cor_test.md)
  (and hence,
  [`correlation()`](https://easystats.github.io/correlation/reference/correlation.md))
  now default the `winsorize` argument to `.1` when it’s set to `TRUE`.

- The `Method` column in output dataframe is now more explicit about the
  correlation method used.

### Bug Fixes

- Winsorization doesn’t fail when `NA`s are present
  ([\#130](https://github.com/easystats/correlation/issues/130)).

### Minor Changes

- Fixed CRAN check issues due to changes in dependent packages.

## correlation 0.5.0

CRAN release: 2020-12-02

### Changes

- Added
  [`winsorize()`](https://easystats.github.io/datawizard/reference/winsorize.html)
  function.

- Added `winsorize` argument for Winsorized correlations.

- Added `method = "somers"` to
  [`correlation()`](https://easystats.github.io/correlation/reference/correlation.md),
  to compute Somers’s Dxy rank correlation for binary outcomes.

- New function
  [`display()`](https://easystats.github.io/insight/reference/display.html),
  to print output into different formats. Currently, only markdown is
  supported.
  [`print_md()`](https://easystats.github.io/insight/reference/display.html)
  is an alias for `display(format = "markdown")`.

### Bug fixes

- Fix bug in
  [`cor_to_p()`](https://easystats.github.io/correlation/reference/cor_to_p.md)
  that gave slightly different test statistics.

## correlation 0.4.0

CRAN release: 2020-09-27

### Changes

- Don’t error if less than 3 valid observations
  ([\#100](https://github.com/easystats/correlation/issues/100)).

- Add “gaussian” rank method.

- Add “gamma” method.

- Add “hoeffding” method.

- Add “blomqvist” method.

### Bug fixes

- Added `Method` column to Bayesian correlations.

- Fix bug when `robust=TRUE`
  ([\#87](https://github.com/easystats/effectsize/issues/87)).

## correlation 0.3.0

CRAN release: 2020-06-19

### Changes

### Bug fixes

## correlation 0.2.1

CRAN release: 2020-05-05

### Changes

- Added confidence intervals CI support for Spearman and Kendall
  ([\#80](https://github.com/easystats/correlation/issues/80))

- Improved documentation
  ([\#45](https://github.com/easystats/correlation/issues/45),
  [\#63](https://github.com/easystats/correlation/issues/63))

### Bug fixes

- Removed CI threshold column from
  [`distance_mahalanobis()`](https://easystats.github.io/correlation/reference/correlation-deprecated.md)

- Fixed bug ([\#76](https://github.com/easystats/correlation/issues/76))

## correlation 0.2.0

CRAN release: 2020-04-09

### Changes

- Some changes were made.

### Bug fixes

- Some bugs were fixed.

## correlation 0.1.0

CRAN release: 2020-03-16

### Changes

- Initial CRAN release.

- Add [`plot()`](https://rdrr.io/r/graphics/plot.default.html)-method
  for [`summary()`](https://rdrr.io/r/base/summary.html).

### Bug fixes

- Fixed issue in
  [`correlation()`](https://easystats.github.io/correlation/reference/correlation.md)
  for some edge cases when `include_factors = TRUE`.

- Fixed issue in
  [`correlation()`](https://easystats.github.io/correlation/reference/correlation.md)
  for correlation coefficients with less than four complete pairs of
  observations (in such cases,
  [`cor_test()`](https://easystats.github.io/correlation/reference/cor_test.md)
  now returns `NA` for the confidence intervals).
