# Factor weights

Factor weights to turn the generalized mean of a product into the
product of generalized means. Useful for price-updating the weights in a
generalized-mean index.

## Usage

``` r
factor_weights(r)

update_weights(x, w = NULL)
```

## Arguments

- r:

  A finite number giving the order of the generalized mean.

- x:

  A strictly positive numeric vector.

- w:

  A strictly positive numeric vector of weights, the same length as `x`.
  The default is to equally weight each element of `x`.

## Value

`factor_weights()` return a function:

    function(x, w = NULL){...}

`update_weights()` returns a numeric vector the same length as `x`.

## Details

The function `factor_weights(r)` returns a function to compute weights
`u(x, w)` such that

    generalized_mean(r)(x * y, w) ==
      generalized_mean(r)(x, w) * generalized_mean(r)(y, u(x, w))

This generalizes the result in section C.5 of Chapter 9 of the PPI
Manual for chaining the Young index, and gives a way to chain
generalized-mean price indexes over time.

Factoring weights with `r = 1` sometimes gets called price-updating
weights; `update_weights()` simply calls `factor_weights(1)()`.

Factoring weights return a value that is the same length as `x`, so any
missing values in `x` or the weights will return `NA`. Unless all values
are `NA`, however, the result will still satisfy the above identity when
`na.rm = TRUE`.

## References

ILO, IMF, OECD, UNECE, and World Bank. (2004). *Producer Price Index
Manual: Theory and Practice*. International Monetary Fund.

## See also

[`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md)
for the generalized mean.

[`grouped()`](https://marberts.github.io/gpindex/reference/grouped.md)
to make these functions operate on grouped data.

Other weights functions:
[`scale_weights()`](https://marberts.github.io/gpindex/reference/scale_weights.md),
[`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)

## Examples

``` r
x <- 1:3
y <- 4:6
w <- 3:1
# Factor the harmonic mean by chaining the calculation.

harmonic_mean(x * y, w)
#> [1] 5.966851
harmonic_mean(x, w) * harmonic_mean(y, factor_weights(-1)(x, w))
#> [1] 5.966851

# The common case of an arithmetic mean.

arithmetic_mean(x * y, w)
#> [1] 8.333333
arithmetic_mean(x, w) * arithmetic_mean(y, update_weights(x, w))
#> [1] 8.333333

# In cases where x and y have the same order, Chebyshev's
# inequality implies that the chained calculation is too small.

arithmetic_mean(x * y, w) >
  arithmetic_mean(x, w) * arithmetic_mean(y, w)
#> [1] TRUE
```
