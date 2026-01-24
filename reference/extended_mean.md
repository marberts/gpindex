# Extended mean

Calculate a generalized logarithmic mean / extended mean.

## Usage

``` r
extended_mean(r, s)

generalized_logmean(r)

logmean(a, b, tol = .Machine$double.eps^0.5)
```

## Arguments

- r, s:

  A finite number giving the order of the generalized logarithmic mean /
  extended mean.

- a, b:

  A strictly positive numeric vector.

- tol:

  The tolerance used to determine if `a == b`.

## Value

`generalized_logmean()` and `extended_mean()` return a function:

    function(a, b, tol = .Machine$double.eps^0.5){...}

`logmean()` returns a numeric vector, the same length as
`max(length(a), length(b))`, giving the component-wise logarithmic mean
of `a` and `b`.

## Details

The function `extended_mean()` returns a function to compute the
component-wise extended mean of `a` and `b` of orders `r` and `s`. See
Bullen (2003, p. 393) for a definition. This is also called the
difference mean, Stolarsky mean, or extended mean-value mean.

The function `generalized_logmean()` returns a function to compute the
component-wise generalized logarithmic mean of `a` and `b` of order `r`.
See Bullen (2003, p. 385) for a definition. The generalized logarithmic
mean is a special case of the extended mean, corresponding to
`extended_mean(r, 1)()`, but is more commonly used for price indexes.

The function `logmean()` returns the ordinary component-wise logarithmic
mean of `a` and `b`, and corresponds to `generalized_logmean(1)()`.

Both `a` and `b` should be strictly positive. This is not enforced, but
the results may not make sense when the generalized logarithmic mean /
extended mean is not defined. The usual recycling rules apply when `a`
and `b` are not the same length.

By definition, the generalized logarithmic mean / extended mean of `a`
and `b` is `a` when `a == b`. The `tol` argument is used to test
equality by checking if `abs(a - b) <= tol`. The default value is the
same as [`all.equal()`](https://rdrr.io/r/base/all.equal.html). In some
cases it's useful to multiply `tol` by a scale factor, such as
`max(abs(a), abs(b))`. This often doesn't matter when making price
indexes, however, as `a` and `b` are usually around 1.

## Note

`generalized_logmean()` can be defined on the extended real line, so
that `r = -Inf / Inf` returns
[`pmin()`](https://rdrr.io/r/base/Extremes.html)/[`pmax()`](https://rdrr.io/r/base/Extremes.html),
to agree with the definition in, e.g., Bullen (2003). This is not
implemented, and `r` must be finite.

## References

Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
Springer Science+Business Media.

## See also

[`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
uses the extended mean to turn a generalized mean of order \\r\\ into a
generalized mean of order \\s\\.

Other means:
[`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md),
[`lehmer_mean()`](https://marberts.github.io/gpindex/reference/lehmer_mean.md),
[`nested_mean()`](https://marberts.github.io/gpindex/reference/nested_mean.md)

## Examples

``` r
x <- 8:5
y <- 1:4

# The arithmetic and geometric means are special cases of the
# generalized logarithmic mean.

all.equal(generalized_logmean(2)(x, y), (x + y) / 2)
#> [1] TRUE
all.equal(generalized_logmean(-1)(x, y), sqrt(x * y))
#> [1] TRUE

# The harmonic mean cannot be expressed as a logarithmic mean, but can
# be expressed as an extended mean.

all.equal(extended_mean(-2, -1)(x, y), 2 / (1 / x + 1 / y))
#> [1] TRUE

# The quadratic mean is also a type of extended mean.

all.equal(extended_mean(2, 4)(x, y), sqrt(x^2 / 2 + y^2 / 2))
#> [1] TRUE

# As are heronian and centroidal means.

all.equal(
  extended_mean(0.5, 1.5)(x, y),
  (x + sqrt(x * y) + y) / 3
)
#> [1] TRUE
all.equal(
  extended_mean(2, 3)(x, y),
  2 / 3 * (x^2 + x * y + y^2) / (x + y)
)
#> [1] TRUE
```
