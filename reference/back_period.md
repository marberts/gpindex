# Offset a vector prices or quantities

For each product, compute either the position of the previous period
(back period), or the position of the first period (base period). Useful
when price information is stored in a table.

## Usage

``` r
back_period(period, product = gl(1, length(period)), match_first = TRUE)

base_period(period, product = gl(1, length(period)), match_first = TRUE)
```

## Arguments

- period:

  A factor, or something that can be coerced into one, that gives the
  time period for each transaction. The ordering of time periods follows
  the levels of `period` to agree with
  [`cut()`](https://rdrr.io/r/base/cut.POSIXt.html).

- product:

  A factor, or something that can be coerced into one, that gives the
  product identifier for each transaction. The default is to assume that
  all transactions are for the same product.

- match_first:

  Should products in the first period match with themselves (the
  default)?

## Value

Both functions return a numeric vector of indices for the back/base
periods. With `back_period()`, for all periods after the first, the
resulting vector gives the location of the corresponding product in the
previous period. With `base_period()`, the resulting vector gives the
location of the corresponding product in the first period. The locations
are unchanged for the first time period if `match_first = TRUE`, `NA`
otherwise.

## Note

By definition, there must be at most one transaction for each product in
each time period to determine a back/base period. If multiple
transactions correspond to a period-product pair, then the back/base
period at a point in time is always the first position for that product
in the previous period.

## See also

[outliers](https://marberts.github.io/gpindex/reference/outliers.md) for
common methods to detect outliers for price relatives.

`rs_pairs` in the rsmatrix package for making sales pairs.

## Examples

``` r
df <- data.frame(
  price = 1:6,
  product = factor(c("a", "b")),
  period = factor(c(1, 1, 2, 2, 3, 3))
)

with(df, back_period(period, product))
#> [1] 1 2 1 2 3 4

# Make period-over-period price relatives.

with(df, price / price[back_period(period, product)])
#> [1] 1.000000 1.000000 3.000000 2.000000 1.666667 1.500000

# Make fixed-base price relatives.

with(df, price / price[base_period(period, product)])
#> [1] 1 1 3 2 5 3

# Change the base period with relevel().

with(df, price / price[base_period(relevel(period, "2"), product)])
#> [1] 0.3333333 0.5000000 1.0000000 1.0000000 1.6666667 1.5000000

# Warning is given if the same product has multiple prices
# at any point in time.

with(df, back_period(period))
#> Warning: there are duplicated period-product pairs
#> [1] 1 1 1 1 3 3
```
