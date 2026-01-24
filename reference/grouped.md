# Grouped operator

Make a function applicable to grouped data.

## Usage

``` r
grouped(f, ...)
```

## Arguments

- f:

  A function.

- ...:

  Deprecated. Additional arguments to `f` that should *not* be treated
  as grouped.

## Value

A function like `f` with a new argument `group`. This accepts a factor
to split all other arguments in `f` into groups before applying `f` to
each group and combining the results. It is similar to
[`ave()`](https://rdrr.io/r/stats/ave.html), but more general.

## See also

Other operators:
[`balanced()`](https://marberts.github.io/gpindex/reference/balanced.md),
[`quantity_index()`](https://marberts.github.io/gpindex/reference/quantity_index.md)

## Examples

``` r
# Redistribute weights.

x <- 1:6
w <- c(1:5, NA)
f <- factor(rep(letters[1:2], each = 3))
w1 <- c(2, 4)
w2 <- 1:6

harmonic_mean(mapply(harmonic_mean, split(x, f), split(w2, f)), w1)
#> [1] 3.333333

wr <- grouped(scale_weights)(w2, group = f) * w1[f]
harmonic_mean(x, wr)
#> [1] 3.333333
```
