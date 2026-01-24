# Generalized mean

Calculate a weighted generalized mean.

## Usage

``` r
generalized_mean(r)

arithmetic_mean(x, w = NULL, na.rm = FALSE)

geometric_mean(x, w = NULL, na.rm = FALSE)

harmonic_mean(x, w = NULL, na.rm = FALSE)
```

## Arguments

- r:

  A finite number giving the order of the generalized mean.

- x:

  A strictly positive numeric vector.

- w:

  A strictly positive numeric vector of weights, the same length as `x`.
  The default is to equally weight each element of `x`.

- na.rm:

  Should missing values in `x` and `w` be removed? By default missing
  values in `x` or `w` return a missing value.

## Value

`generalized_mean()` returns a function:

    function(x, w = NULL, na.rm = FALSE){...}

`arithmetic_mean()`, `geometric_mean()`, and `harmonic_mean()` each
return a numeric value for the generalized means of order 1, 0, and -1.

## Details

The function `generalized_mean()` returns a function to compute the
generalized mean of `x` with weights `w` and exponent `r` (i.e.,
\\\prod\_{i = 1}^{n} x\_{i}^{w\_{i}}\\ when \\r = 0\\ and
\\\left(\sum\_{i = 1}^{n} w\_{i} x\_{i}^{r}\right)^{1 / r}\\ otherwise).
This is also called the power mean, Hölder mean, or \\l_p\\ mean; see
Bullen (2003, p. 175) for details.

The functions `arithmetic_mean()`, `geometric_mean()`, and
`harmonic_mean()` compute the arithmetic, geometric, and harmonic (or
subcontrary) means, also known as the Pythagorean means. These are the
most useful means for making price indexes, and correspond to setting
`r = 1`, `r = 0`, and `r = -1` in `generalized_mean()`.

Both `x` and `w` should be strictly positive (and finite), especially
for the purpose of making a price index. This is not enforced, but the
results may not make sense if the generalized mean is not defined. There
are two exceptions to this.

1.  The convention in Hardy et al. (1952, p. 13) is used in cases where
    `x` has zeros: the generalized mean is 0 whenever `w` is strictly
    positive and `r` \< 0. (The analogous convention holds whenever at
    least one element of `x` is `Inf`: the generalized mean is `Inf`
    whenever `w` is strictly positive and `r` \> 0.)

2.  Some authors let `w` be non-negative and sum to 1 (e.g., Sydsaeter
    et al., 2005, p. 47). If `w` has zeros, then the corresponding
    element of `x` has no impact on the mean whenever `x` is strictly
    positive. Unlike
    [`weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html),
    however, zeros in `w` are not strong zeros, so infinite values in
    `x` will propagate even if the corresponding elements of `w` are
    zero.

The weights are scaled to sum to 1 to satisfy the definition of a
generalized mean. There are certain price indexes where the weights
should not be scaled (e.g., the Vartia-I index); use
[`sum()`](https://rdrr.io/r/base/sum.html) for these cases.

The underlying calculation returned by `generalized_mean()` is mostly
identical to
[`weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html), with
one important exception: missing values in the weights are not treated
differently than missing values in `x`. Setting `na.rm = TRUE` drops
missing values in both `x` and `w`, not just `x`. This ensures that
certain useful identities are satisfied with missing values in `x`. In
most cases `arithmetic_mean()` is a drop-in replacement for
[`weighted.mean()`](https://rdrr.io/r/stats/weighted.mean.html).

## Note

`generalized_mean()` can be defined on the extended real line, so that
`r = -Inf / Inf` returns
[`min()`](https://rdrr.io/r/base/Extremes.html)/[`max()`](https://rdrr.io/r/base/Extremes.html),
to agree with the definition in, e.g., Bullen (2003). This is not
implemented, and `r` must be finite.

There are a number of existing functions for calculating *unweighted*
geometric and harmonic means, namely the `geometric.mean()` and
`harmonic.mean()` functions in the psych package, the `geomean()`
function in the FSA package, the `GMean()` and `HMean()` functions in
the DescTools package, and the `geoMean()` function in the EnvStats
package. Similarly, the `ci_generalized_mean()` function in the Compind
package calculates an *unweighted* generalized mean.

## References

Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
Springer Science+Business Media.

Fisher, I. (1922). *The Making of Index Numbers*. Houghton Mifflin
Company.

Hardy, G., Littlewood, J. E., and Polya, G. (1952). *Inequalities* (2nd
edition). Cambridge University Press.

IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

Lord, N. (2002). Does Smaller Spread Always Mean Larger Product? *The
Mathematical Gazette*, 86(506): 273-274.

Sydsaeter, K., Strom, A., and Berck, P. (2005). *Economists'
Mathematical Manual* (4th edition). Springer.

## See also

[`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
transforms the weights to turn a generalized mean of order \\r\\ into a
generalized mean of order \\s\\.

[`factor_weights()`](https://marberts.github.io/gpindex/reference/factor_weights.md)
calculates the weights to factor a mean of products into a product of
means.

[price_indexes](https://marberts.github.io/gpindex/reference/price_indexes.md)
and
[`quantity_index()`](https://marberts.github.io/gpindex/reference/quantity_index.md)
for simple wrappers that use `generalized_mean()` to calculate common
indexes.

[`back_period()`](https://marberts.github.io/gpindex/reference/back_period.md)/[`base_period()`](https://marberts.github.io/gpindex/reference/back_period.md)
for a simple utility function to turn prices in a table into price
relatives.

Other means:
[`extended_mean()`](https://marberts.github.io/gpindex/reference/extended_mean.md),
[`lehmer_mean()`](https://marberts.github.io/gpindex/reference/lehmer_mean.md),
[`nested_mean()`](https://marberts.github.io/gpindex/reference/nested_mean.md)

## Examples

``` r
x <- 1:3
w <- c(0.25, 0.25, 0.5)

# The dispersion between the arithmetic, geometric, and harmonic
# mean usually increases as the variance of 'x' increases.

x <- c(1, 3, 5)
y <- c(2, 3, 4)

var(x) > var(y)
#> [1] TRUE

arithmetic_mean(x) - geometric_mean(x)
#> [1] 0.5337879
arithmetic_mean(y) - geometric_mean(y)
#> [1] 0.1155009

geometric_mean(x) - harmonic_mean(x)
#> [1] 0.5096903
geometric_mean(y) - harmonic_mean(y)
#> [1] 0.1152684

# But the dispersion between these means is only bounded by the
# variance (Bullen, 2003, p. 156).

arithmetic_mean(x) - geometric_mean(x) >= 2 / 3 * var(x) / (2 * max(x))
#> [1] TRUE
arithmetic_mean(x) - geometric_mean(x) <= 2 / 3 * var(x) / (2 * min(x))
#> [1] TRUE

# Example by Lord (2002) where the dispersion decreases as the variance
# increases, counter to the claims by Fisher (1922, p. 108) and the
# CPI manual (par. 1.14)

x <- (5 + c(sqrt(5), -sqrt(5), -3)) / 4
y <- (16 + c(7 * sqrt(2), -7 * sqrt(2), 0)) / 16

var(x) > var(y)
#> [1] TRUE

arithmetic_mean(x) - geometric_mean(x)
#> [1] 0.145012
arithmetic_mean(y) - geometric_mean(y)
#> [1] 0.1485894

geometric_mean(x) - harmonic_mean(x)
#> [1] 0.104988
geometric_mean(y) - harmonic_mean(y)
#> [1] 0.1439479

# The "bias" in the arithmetic and harmonic indexes is also smaller in
# this case, counter to the claim by Fisher (1922, p. 108)

arithmetic_mean(x) * arithmetic_mean(1 / x) - 1
#> [1] 0.3333333
arithmetic_mean(y) * arithmetic_mean(1 / y) - 1
#> [1] 0.4135021

harmonic_mean(x) * harmonic_mean(1 / x) - 1
#> [1] -0.25
harmonic_mean(y) * harmonic_mean(1 / y) - 1
#> [1] -0.2925373
```
