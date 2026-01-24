# Nested generalized mean

Calculate the (outer) generalized mean of two (inner) generalized means
(i.e., crossing generalized means).

## Usage

``` r
nested_mean(r1, r2, t = c(1, 1))

fisher_mean(x, w1 = NULL, w2 = NULL, na.rm = FALSE)
```

## Arguments

- r1:

  A finite number giving the order of the outer generalized mean.

- r2:

  A pair of finite numbers giving the order of the inner generalized
  means.

- t:

  A pair of strictly positive weights for the inner generalized means.
  The default is equal weights.

- x:

  A strictly positive numeric vector.

- w1, w2:

  A strictly positive numeric vector of weights, the same length as `x`.
  The default is to equally weight each element of `x`.

- na.rm:

  Should missing values in `x`, `w1`, and `w2` be removed? By default
  missing values in `x`, `w1`, or `w2` return a missing value.

## Value

`nested_mean()` returns a function:

    function(x, w1 = NULL, w2 = NULL, na.rm = FALSE){...}

This computes the generalized mean of order `r1` of the generalized mean
of order `r2[1]` of `x` with weights `w1` and the generalized mean of
order `r2[2]` of `x` with weights `w2`.

`fisher_mean()` returns a numeric value for the geometric mean of the
arithmetic and harmonic means (i.e., `r1 = 0` and `r2 = c(1, -1)`).

## Note

There is some ambiguity about how to remove missing values in `w1` or
`w2` when `na.rm = TRUE`. The approach here is to remove missing values
when calculating each of the inner means individually, rather than
removing all missing values prior to any calculations. This means that a
different number of data points could be used to calculate the inner
means. Use the
[`balanced()`](https://marberts.github.io/gpindex/reference/balanced.md)
operator to balance missing values across `w1` and `w2` prior to any
calculations.

## References

ILO, IMF, OECD, UNECE, and World Bank. (2004). *Producer Price Index
Manual: Theory and Practice*. International Monetary Fund.

## See also

[`nested_contributions()`](https://marberts.github.io/gpindex/reference/contributions.md)
for percent-change contributions for indexes based on nested generalized
means, like the Fisher index.

Other means:
[`extended_mean()`](https://marberts.github.io/gpindex/reference/extended_mean.md),
[`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md),
[`lehmer_mean()`](https://marberts.github.io/gpindex/reference/lehmer_mean.md)

## Examples

``` r
x <- 1:3
w1 <- 4:6
w2 <- 7:9

# A function to make the superlative quadratic mean price index as
# a product of generalized means.

quadratic_mean_index <- function(r) nested_mean(0, c(r / 2, -r / 2))

quadratic_mean_index(2)(x, w1, w2)
#> [1] 1.912366

fisher_mean(x, w1, w2)
#> [1] 1.912366

# The (arithmetic) Walsh index is the implicit price index when using a
# superlative quadratic mean quantity index of order 1.

p2 <- price6[[2]]
p1 <- price6[[1]]
q2 <- quantity6[[2]]
q1 <- quantity6[[1]]

walsh <- quadratic_mean_index(1)

sum(p2 * q2) / sum(p1 * q1) / walsh(q2 / q1, p1 * q1, p2 * q2)
#> [1] 1.401718

sum(p2 * sqrt(q2 * q1)) / sum(p1 * sqrt(q2 * q1))
#> [1] 1.401718

# Counter to the PPI manual (par. 1.105), it is not a superlative
# quadratic mean price index of order 1.

walsh(p2 / p1, p1 * q1, p2 * q2)
#> [1] 1.401534

# That requires using the average value share as weights.

walsh_weights <- sqrt(scale_weights(p1 * q1) * scale_weights(p2 * q2))
walsh(p2 / p1, walsh_weights, walsh_weights)
#> [1] 1.401718
```
