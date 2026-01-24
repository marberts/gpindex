# Lehmer mean

Calculate a weighted Lehmer mean.

## Usage

``` r
lehmer_mean(r)

contraharmonic_mean(x, w = NULL, na.rm = FALSE)
```

## Arguments

- r:

  A finite number giving the order of the Lehmer mean.

- x:

  A strictly positive numeric vector.

- w:

  A strictly positive numeric vector of weights, the same length as `x`.
  The default is to equally weight each element of `x`.

- na.rm:

  Should missing values in `x` and `w` be removed? By default missing
  values in `x` or `w` return a missing value.

## Value

`lehmer_mean()` returns a function:

    function(x, w = NULL, na.rm = FALSE){...}

`contraharmonic_mean()` returns a numeric value for the Lehmer mean of
order 2.

## Details

The function `lehmer_mean()` returns a function to compute the Lehmer
mean of order `r` of `x` with weights `w`, which is calculated as the
arithmetic mean of `x` with weights \\wx^{r-1}\\. This is also called
the counter-harmonic mean or generalized anti-harmonic mean. See Bullen
(2003, p. 245) for details.

The Lehmer mean of order 2 is sometimes called the contraharmonic (or
anti-harmonic) mean. The function `contraharmonic_mean()` simply calls
`lehmer_mean(2)()`. See von der Lippe (2015) for more details on the use
of these means for making price indexes.

## Note

`lehmer_mean()` can be defined on the extended real line, so that
`r = -Inf / Inf` returns
[`min()`](https://rdrr.io/r/base/Extremes.html)/[`max()`](https://rdrr.io/r/base/Extremes.html),
to agree with the definition in, e.g., Bullen (2003). This is not
implemented, and `r` must be finite.

## References

Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
Springer Science+Business Media.

von der Lippe, P. (2015). Generalized Statistical Means and New Price
Index Formulas, Notes on some unexplored index formulas, their
interpretations and generalizations. Munich Personal RePEc Archive paper
no. 64952.

## See also

Other means:
[`extended_mean()`](https://marberts.github.io/gpindex/reference/extended_mean.md),
[`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md),
[`nested_mean()`](https://marberts.github.io/gpindex/reference/nested_mean.md)

## Examples

``` r
x <- 2:3
w <- c(0.25, 0.75)

# The Pythagorean means are special cases of the Lehmer mean.

all.equal(lehmer_mean(1)(x, w), arithmetic_mean(x, w))
#> [1] TRUE
all.equal(lehmer_mean(0)(x, w), harmonic_mean(x, w))
#> [1] TRUE
all.equal(lehmer_mean(0.5)(x), geometric_mean(x))
#> [1] TRUE

# When r < 1, the generalized mean is larger than the corresponding
# Lehmer mean.

lehmer_mean(-1)(x, w) < generalized_mean(-1)(x, w)
#> [1] TRUE

# The reverse is true when r > 1.

lehmer_mean(3)(x, w) > generalized_mean(3)(x, w)
#> [1] TRUE

# This implies the contraharmonic mean is larger than the quadratic
# mean, and therefore the Pythagorean means.

contraharmonic_mean(x, w) > arithmetic_mean(x, w)
#> [1] TRUE
contraharmonic_mean(x, w) > geometric_mean(x, w)
#> [1] TRUE
contraharmonic_mean(x, w) > harmonic_mean(x, w)
#> [1] TRUE

# ... and the logarithmic mean

contraharmonic_mean(2:3) > logmean(2, 3)
#> [1] TRUE

# The difference between the arithmetic mean and contraharmonic mean
# is proportional to the variance of x.

weighted_var <- function(x, w) {
  arithmetic_mean((x - arithmetic_mean(x, w))^2, w)
}

arithmetic_mean(x, w) + weighted_var(x, w) / arithmetic_mean(x, w)
#> [1] 2.818182
contraharmonic_mean(x, w)
#> [1] 2.818182
```
