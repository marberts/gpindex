# Quantity index operator

Remaps price arguments into quantity argument (and vice versa) to turn a
price index into a quantity index.

## Usage

``` r
quantity_index(f)
```

## Arguments

- f:

  A [price-index
  function](https://marberts.github.io/gpindex/reference/price_indexes.md).

## Value

A function like `f`, except that the role of prices/quantities is
reversed.

## See also

Other operators:
[`balanced()`](https://marberts.github.io/gpindex/reference/balanced.md),
[`grouped()`](https://marberts.github.io/gpindex/reference/grouped.md)

## Examples

``` r
p1 <- price6[[3]]
p0 <- price6[[2]]
q1 <- quantity6[[3]]
q0 <- quantity6[[2]]

# Remap argument names to be quantities rather than prices.

quantity_index(laspeyres_index)(q1 = q1, q0 = q0, p0 = p0)
#> [1] 1.175887

laspeyres_index(p1 = q1, p0 = q0, q0 = p0)
#> [1] 1.175887

# Works with the index_weights() functions, too.

quantity_index(index_weights("Laspeyres"))(q0 = q0, p0 = p0)
#> [1] 0.96 2.70 2.47 0.91 6.58 0.48
```
