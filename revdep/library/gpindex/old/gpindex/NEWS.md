# Version 0.4.3

- `back_price()` and `base_price()` are deprecated in favor of the more general `back_period()` and `base_period()` functions. They will be removed in a future version.

- The algorithm for making GEKS indexes is now much faster with a rolling window.

# Version 0.4.2

- The functions and overall structure of the package should be fairly stable from now on.

- Added `nested_transmute()` and `nested_transmute2()` for transmuting the weights for nested generalized means. To be consistent with argument names, the first two arguments for `nested_mean()` and `nested_contributions*()` are now `r1` and `r2`.

- Added the geometric Theil and Rao indexes.

# Version 0.3.9

- Added `back_period()` and `base_period()`, which are more general than `back_price()` and `base_price()`.

- Added `lehr_index()`.

- Fixed a rare warning about `sqrt()` making NaNs in `generalized_logmean(-1)` when some inputs were close but not equal, despite no NaNs showing in the result.

- The `lm_index()` and `*_agmean_index()` functions are now function factories.

# Version 0.3.6

- Added the `balanced()` operator to make it easier to remove NAs with price index functions.

- Added the `geks()` function for using price-index function (e.g., `fisher_index()`) to makes a GEKS index.

# Version 0.3.4

- Added French translations.

- Made a number of optimizations to make the results of `generalized_mean()`, `extended_mean()`, `lehmer_mean()`, `transmute_weights()`, and `factor_weights()` faster in common cases.

- Added the `grouped()` operator to make all functions work with grouped data.

# Version 0.3.1

- Most function names have changed to be less awkward; e.g., `mean_generalized()` is now `generalized_mean()`, and `contributions_geometric()` is now `geometric_contributions()`. This is unfortunately not backwards compatible, but needed to be done.

- Added the `nested_mean()` function to calculate nested generalized means for, e.g., the Fisher index.

- The interface for `nested_contributions()` is now much simpler, and the function is focused on making contributions for Fisher indexes. Added the `nested_contributions2()` function that implements a different algorithm.

- Added the `arithmetic_agmean_index()` and `geometric_agmean_index()` functions to calculate the AG mean index.

- Added some functions for standard outlier-detection methods for price relatives.

- Dropped the `scale` argument for `generalized_mean()`, as it really wasn't needed and had the potential to make more problems than it solved.
