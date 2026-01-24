# Balanced operator

Makes a function balance the removal of `NA`s across multiple input
vectors.

## Usage

``` r
balanced(f, ...)
```

## Arguments

- f:

  A function.

- ...:

  Deprecated. Additional arguments to `f` that should *not* be balanced.

## Value

A function like `f` with a new argument `na.rm`. If `na.rm = TRUE` then
[`complete.cases()`](https://rdrr.io/r/stats/complete.cases.html) is
used to remove missing values across all inputs prior to calling `f`.

## See also

Other operators:
[`grouped()`](https://marberts.github.io/gpindex/reference/grouped.md),
[`quantity_index()`](https://marberts.github.io/gpindex/reference/quantity_index.md)

## Examples

``` r
p2 <- price6[[3]]
p1 <- price6[[2]]
q2 <- quantity6[[3]]
q1 <- quantity6[[2]]

# Balance missing values for a Fisher index.

fisher <- balanced(fisher_index)
fisher(p2, p1, q2, replace(q1, 3, NA), na.rm = TRUE)
#> [1] 0.9015449
fisher_index(p2[-3], p1[-3], q2[-3], q1[-3])
#> [1] 0.9015449
```
