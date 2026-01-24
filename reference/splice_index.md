# Splice an index series

Splice a collection of index series computed over a rolling window into
one index series. Splicing on multiple points combines the results with
a geometric mean.

## Usage

``` r
splice_index(x, periods = NULL, initial = NULL, published = FALSE)
```

## Arguments

- x:

  A list of equal-length numeric vectors giving the period-over-period
  indexes for each window.

- periods:

  An integer vector giving the splice points for each window. The
  default splices on each point in the window.

- initial:

  A numeric vector giving an initial period-over-period index series
  onto which the elements of `x` are spliced. The default uses the first
  element of `x`.

- published:

  Should the splice be done against the published series? The default
  splices using the recalculated index series.

## Value

A numeric vector giving the spliced (fixed-base) index series.

## References

Chessa, A. G. (2019). *A Comparison of Index Extension Methods for
Multilateral Methods.* Paper presented at the 16th Meeting of the Ottawa
Group on Price Indices, 8-10 May 2019, Rio de Janeiro, Brazil.

Krsinich, F. (2016). The FEWS index: Fixed effects with a window splice.
*Journal of Official Statistics*, 32(2), 375-404.

## See also

Other price index functions:
[`geks()`](https://marberts.github.io/gpindex/reference/geks.md),
[`index_weights()`](https://marberts.github.io/gpindex/reference/index_weights.md),
[`price_indexes`](https://marberts.github.io/gpindex/reference/price_indexes.md)

## Examples

``` r
# Make an index series over a rolling window.

x <- list(c(1.1, 0.9, 1.2), c(0.8, 1.3, 1.4), c(1.3, 1.3, 0.8))

# Mean splice.

splice_index(x)
#> [1] 1.100000 0.990000 1.188000 1.686819 1.284405

# Movement splice.

splice_index(x, 3)
#> [1] 1.10000 0.99000 1.18800 1.66320 1.33056

# Window splice.

splice_index(x, 1)
#> [1] 1.10000 0.99000 1.18800 1.60160 1.18976

# Splicing on the published series preserves the within-window
# movement of the index series.

splice_index(x, 1, published = TRUE)
#> [1] 1.10000 0.99000 1.18800 1.60160 1.33848
```
