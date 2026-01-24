# Transmute weights

Transmute weights to turn a generalized mean of order \\r\\ into a
generalized mean of order \\s\\. Useful for calculating additive and
multiplicative decompositions for a generalized-mean index, and those
made of nested generalized means (e.g., Fisher index).

## Usage

``` r
transmute_weights(r, s)

nested_transmute(r1, r2, s, t = c(1, 1))

nested_transmute2(r1, r2, s, t = c(1, 1))
```

## Arguments

- r, s:

  A finite number giving the order of the generalized mean. See details.

- r1:

  A finite number giving the order of the outer generalized mean.

- r2:

  A pair of finite numbers giving the order of the inner generalized
  means.

- t:

  A pair of strictly positive weights for the inner generalized means.
  The default is equal weights.

## Value

`transmute_weights()` returns a function:

    function(x, w = NULL, tol = .Machine$double.eps^0.5){...}

`nested_transmute()` and `nested_transmute2()` similarly return a
function:

    function(x, w1 = NULL, w2 = NULL,
             tol = .Machine$double.eps^0.5){...}

## Details

The function `transmute_weights(r, s)` returns a function to compute a
vector of weights `v(x, w)` such that

    generalized_mean(r)(x, w) == generalized_mean(s)(x, v(x, w))

`nested_transmute(r1, r2, t, s)` and `nested_transmute2(r1, r2, t, s)`
do the same for nested generalized means, so that

    nested_mean(r1, r2, t)(x, w1, w2) ==
      generalized_mean(s)(x, v(x, w1, w2))

Transmuting weights returns a value that is the same length as `x`, so
any missing values in `x` or the weights will return `NA`. Unless all
values are `NA`, however, the result will still satisfy the above
identities when `na.rm = TRUE`.

## References

See
[`vignette("decomposing-indexes")`](https://marberts.github.io/gpindex/articles/decomposing-indexes.md)
for more details.

## See also

[`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md)
for the generalized mean and
[`nested_mean()`](https://marberts.github.io/gpindex/reference/nested_mean.md)
for the nested mean.

[`extended_mean()`](https://marberts.github.io/gpindex/reference/extended_mean.md)
for the extended mean that underlies `transmute_weights()`.

[`contributions()`](https://marberts.github.io/gpindex/reference/contributions.md)
for calculating additive percent-change contributions.

[`grouped()`](https://marberts.github.io/gpindex/reference/grouped.md)
to make these functions operate on grouped data.

Other weights functions:
[`factor_weights()`](https://marberts.github.io/gpindex/reference/factor_weights.md),
[`scale_weights()`](https://marberts.github.io/gpindex/reference/scale_weights.md)

## Examples

``` r
x <- 1:3
w <- 3:1

# Calculate the geometric mean as an arithmetic mean and
# harmonic mean by transmuting the weights.

geometric_mean(x)
#> [1] 1.817121
arithmetic_mean(x, transmute_weights(0, 1)(x))
#> [1] 1.817121
harmonic_mean(x, transmute_weights(0, -1)(x))
#> [1] 1.817121

# Works for nested means, too.

w1 <- 3:1
w2 <- 1:3

fisher_mean(x, w1, w2)
#> [1] 1.825742

arithmetic_mean(x, nested_transmute(0, c(1, -1), 1)(x, w1, w2))
#> [1] 1.825742
arithmetic_mean(x, nested_transmute2(0, c(1, -1), 1)(x, w1, w2))
#> [1] 1.825742
```
