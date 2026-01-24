# Changelog

## gpindex (development version)

- Fixed a bug where
  [`hb_transform()`](https://marberts.github.io/gpindex/reference/outliers.md)
  would give an error with missing values.

## gpindex 0.6.3

CRAN release: 2025-06-07

- Bumped minimum version of R to \>= 4.1.

- [`geks()`](https://marberts.github.io/gpindex/reference/geks.md) is
  now faster and uses less memory to makes the GEKS index.

- Time periods with entirely missing data no longer return an index
  value with
  [`geks()`](https://marberts.github.io/gpindex/reference/geks.md),
  fixing [\#8](https://github.com/marberts/gpindex/issues/8).

- Added a new vignette to better explain decomposing an index.

- Added
  [`martini_index()`](https://marberts.github.io/gpindex/reference/price_indexes.md)
  to compute the family of Martini indexes.

- Added
  [`kimber_method()`](https://marberts.github.io/gpindex/reference/outliers.md)
  for outlier detection.

- Outlier detection functions are now stricter about their inputs.

- Functions for transmuting weights now get a `tol` argument to control
  the tolerance in the extended mean.

## gpindex 0.6.2

CRAN release: 2024-08-16

- [`splice_index()`](https://marberts.github.io/gpindex/reference/splice_index.md)
  now keeps names.

- Fixed a bug with
  [`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
  where the weights could be negative.

- Price-index functions have better argument checking.

## gpindex 0.6.1

CRAN release: 2024-04-12

- Updated maintainer email.

- Added a parameter to generalize
  [`geks()`](https://marberts.github.io/gpindex/reference/geks.md) by
  controlling how indexes are averaged over the rolling window.

- Fixed a bug where
  [`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
  and
  [`factor_weights()`](https://marberts.github.io/gpindex/reference/factor_weights.md)
  could return a result with a different length than `w`.

- Added a new function
  [`splice_index()`](https://marberts.github.io/gpindex/reference/splice_index.md)
  for splicing indexes calculated over a rolling window (this was
  previously sketched in an example).

- [`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
  is now faster.

## gpindex 0.6.0

CRAN release: 2023-11-15

- Bumped minimum version of R to at least 4.0.

- The use of `...` in
  [`grouped()`](https://marberts.github.io/gpindex/reference/grouped.md)
  and
  [`balanced()`](https://marberts.github.io/gpindex/reference/balanced.md)
  is deprecated, and will be removed in a future version. The same
  behavior can be had by using an anonymous function.

- Added the
  [`walsh_geks()`](https://marberts.github.io/gpindex/reference/geks.md)
  function.

- [`back_period()`](https://marberts.github.io/gpindex/reference/back_period.md)
  and
  [`base_period()`](https://marberts.github.io/gpindex/reference/back_period.md)
  gain a new argument `match_first` to control whether products in the
  first period match to themselves or return `NA`.

- Updated documentation.

- Added a brief vignette.

## gpindex 0.5.0

CRAN release: 2023-08-08

- `back_price()` and `base_price()` have been removed.

- Functions for transforming weights only keep the attributes of the
  weights (if any), as documented.

- [`grouped()`](https://marberts.github.io/gpindex/reference/grouped.md)
  no longer mangles names.

## gpindex 0.4.3

CRAN release: 2022-05-01

- `back_price()` and `base_price()` are deprecated in favor of the more
  general
  [`back_period()`](https://marberts.github.io/gpindex/reference/back_period.md)
  and
  [`base_period()`](https://marberts.github.io/gpindex/reference/back_period.md)
  functions. They will be removed in a future version.

- The algorithm for making GEKS indexes is now much faster with a
  rolling window.

## gpindex 0.4.2

CRAN release: 2022-01-26

- The functions and overall structure of the package should be fairly
  stable from now on.

- Added
  [`nested_transmute()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
  and
  [`nested_transmute2()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
  for transmuting the weights for nested generalized means. To be
  consistent with argument names, the first two arguments for
  [`nested_mean()`](https://marberts.github.io/gpindex/reference/nested_mean.md)
  and `nested_contributions*()` are now `r1` and `r2`.

- Added the geometric Theil and Rao indexes.

## gpindex 0.3.9

CRAN release: 2021-11-26

- Added
  [`back_period()`](https://marberts.github.io/gpindex/reference/back_period.md)
  and
  [`base_period()`](https://marberts.github.io/gpindex/reference/back_period.md),
  which are more general than `back_price()` and `base_price()`.

- Added
  [`lehr_index()`](https://marberts.github.io/gpindex/reference/price_indexes.md).

- Fixed a rare warning about
  [`sqrt()`](https://rdrr.io/r/base/MathFun.html) making NaNs in
  `generalized_logmean(-1)` when some inputs were close but not equal,
  despite no `NaN`s showing in the result.

- The
  [`lm_index()`](https://marberts.github.io/gpindex/reference/price_indexes.md)
  and `*_agmean_index()` functions are now function factories.

## gpindex 0.3.6

CRAN release: 2021-10-02

- Added the
  [`balanced()`](https://marberts.github.io/gpindex/reference/balanced.md)
  operator to make it easier to remove NAs with price index functions.

- Added the
  [`geks()`](https://marberts.github.io/gpindex/reference/geks.md)
  function for using price-index function (e.g.,
  [`fisher_index()`](https://marberts.github.io/gpindex/reference/price_indexes.md))
  to makes a GEKS index.

## gpindex 0.3.4

CRAN release: 2021-08-04

- Added French translations.

- Made a number of optimizations to make the results of
  [`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md),
  [`extended_mean()`](https://marberts.github.io/gpindex/reference/extended_mean.md),
  [`lehmer_mean()`](https://marberts.github.io/gpindex/reference/lehmer_mean.md),
  [`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md),
  and
  [`factor_weights()`](https://marberts.github.io/gpindex/reference/factor_weights.md)
  faster in common cases.

- Added the
  [`grouped()`](https://marberts.github.io/gpindex/reference/grouped.md)
  operator to make all functions work with grouped data.

## gpindex 0.3.1

CRAN release: 2021-07-07

- Most function names have changed to be less awkward; e.g.,
  `mean_generalized()` is now
  [`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md),
  and `contributions_geometric()` is now
  [`geometric_contributions()`](https://marberts.github.io/gpindex/reference/contributions.md).
  This is unfortunately not backwards compatible, but needed to be done.

- Added the
  [`nested_mean()`](https://marberts.github.io/gpindex/reference/nested_mean.md)
  function to calculate nested generalized means for, e.g., the Fisher
  index.

- The interface for
  [`nested_contributions()`](https://marberts.github.io/gpindex/reference/contributions.md)
  is now much simpler, and the function is focused on making
  contributions for Fisher indexes. Added the
  [`nested_contributions2()`](https://marberts.github.io/gpindex/reference/contributions.md)
  function that implements a different algorithm.

- Added the
  [`arithmetic_agmean_index()`](https://marberts.github.io/gpindex/reference/price_indexes.md)
  and
  [`geometric_agmean_index()`](https://marberts.github.io/gpindex/reference/price_indexes.md)
  functions to calculate the AG mean index.

- Added some functions for standard outlier-detection methods for price
  relatives.

- Dropped the `scale` argument for
  [`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md),
  as it really wasn’t needed and had the potential to make more problems
  than it solved.
