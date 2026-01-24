# Scale weights

Scale a vector of weights so that they sum to 1.

## Usage

``` r
scale_weights(x)
```

## Arguments

- x:

  A strictly positive numeric vector.

## Value

A numeric vector that sums to 1. If there are `NA`s in `x` then the
result sums 1 to if these values are removed.

## See also

[`grouped()`](https://marberts.github.io/gpindex/reference/grouped.md)
to make this function applicable to grouped data.

Other weights functions:
[`factor_weights()`](https://marberts.github.io/gpindex/reference/factor_weights.md),
[`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)

## Examples

``` r
scale_weights(1:5)
#> [1] 0.06666667 0.13333333 0.20000000 0.26666667 0.33333333

scale_weights(c(1:5, NA))
#> [1] 0.06666667 0.13333333 0.20000000 0.26666667 0.33333333         NA
```
