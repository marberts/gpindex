# Percent-change contributions

Calculate additive percent-change contributions for generalized-mean
price indexes, and indexes that nest two levels of generalized means
consisting of an outer generalized mean and two inner generalized means
(e.g., the Fisher index).

## Usage

``` r
contributions(r)

arithmetic_contributions(x, w = NULL)

geometric_contributions(x, w = NULL)

harmonic_contributions(x, w = NULL)

nested_contributions(r1, r2, t = c(1, 1))

nested_contributions2(r1, r2, t = c(1, 1))

fisher_contributions(x, w1 = NULL, w2 = NULL)

fisher_contributions2(x, w1 = NULL, w2 = NULL)
```

## Arguments

- r:

  A finite number giving the order of the generalized mean.

- x:

  A strictly positive numeric vector.

- w, w1, w2:

  A strictly positive numeric vector of weights, the same length as `x`.
  The default is to equally weight each element of `x`.

- r1:

  A finite number giving the order of the outer generalized mean.

- r2:

  A pair of finite numbers giving the order of the inner generalized
  means.

- t:

  A pair of strictly positive weights for the inner generalized means.
  The default is equal weights.

## Value

`contributions()` returns a function:

    function(x, w = NULL){...}

`nested_contributions()` and `nested_contributions2()` return a
function:

    function(x, w1 = NULL, w2 = NULL){...}

`arithmetic_contributions()`, `geometric_contributions()`,
`harmonic_contributions()`, `fisher_contributions()`, and
`fisher_contributions2()` each return a numeric vector, the same length
as `x`.

## Details

The function `contributions()` is a simple wrapper for
[`transmute_weights(r, 1)()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
to calculate (additive) percent-change contributions for a price index
based on a generalized mean of order `r`. It returns a function to
compute a vector `v(x, w)` such that

    generalized_mean(r)(x, w) - 1 == sum(v(x, w))

The `arithmetic_contributions()`, `geometric_contributions()` and
`harmonic_contributions()` functions cover the most important cases
(i.e., `r = 1`, `r = 0`, and `r = -1`).

The `nested_contributions()` and `nested_contributions2()` functions are
the analog of `contributions()` for an index based on a nested
generalized mean with two levels, like a Fisher index. They are wrappers
around
[`nested_transmute()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
and
[`nested_transmute2()`](https://marberts.github.io/gpindex/reference/transmute_weights.md),
respectively.

The `fisher_contributions()` and `fisher_contributions2()` functions
correspond to `nested_contributions(0, c(1, -1))()` and
`nested_contributions2(0, c(1, -1))()`, and are appropriate for
calculating percent-change contributions for a Fisher index.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

Hallerbach, W. G. (2005). An alternative decomposition of the Fisher
index. *Economics Letters*, 86(2):147–152

Reinsdorf, M. B., Diewert, W. E., and Ehemann, C. (2002). Additive
decompositions for Fisher, Törnqvist and geometric mean indexes.
*Journal of Economic and Social Measurement*, 28(1-2):51–61.

## See also

[`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
for the underlying implementation.

## Examples

``` r
p2 <- price6[[2]]
p1 <- price6[[1]]
q2 <- quantity6[[2]]
q1 <- quantity6[[1]]

# Percent-change contributions for the Jevons index.

geometric_mean(p2 / p1) - 1
#> [1] 0.2419201

geometric_contributions(p2 / p1)
#> [1]  0.03330510  0.20397490  0.04799594 -0.06452574  0.06163523 -0.04046534

all.equal(
  geometric_mean(p2 / p1) - 1,
  sum(geometric_contributions(p2 / p1))
)
#> [1] TRUE

# Percent-change contributions for the Fisher index in section 6 of
# Reinsdorf et al. (2002).

(con <- fisher_contributions(p2 / p1, p1 * q1, p2 * q2))
#> [1]  0.01782904  0.18814917  0.05792820 -0.03414299  0.18217457 -0.01088796

all.equal(sum(con), fisher_index(p2, p1, q2, q1) - 1)
#> [1] TRUE

# Not the only way.

(con2 <- fisher_contributions2(p2 / p1, p1 * q1, p2 * q2))
#> [1]  0.01782865  0.18815627  0.05792735 -0.03414531  0.18217135 -0.01088828

all.equal(sum(con2), fisher_index(p2, p1, q2, q1) - 1)
#> [1] TRUE

# The same as the van IJzeren decomposition in section 4.2.2 of
# Balk (2008).

Qf <- quantity_index(fisher_index)(q2, q1, p2, p1)
Ql <- quantity_index(laspeyres_index)(q2, q1, p1)
wl <- scale_weights(p1 * q1)
wp <- scale_weights(p1 * q2)

(Qf / (Qf + Ql) * wl + Ql / (Qf + Ql) * wp) * (p2 / p1 - 1)
#> [1]  0.01782865  0.18815627  0.05792735 -0.03414531  0.18217135 -0.01088828

# Similar to the method in section 2 of Reinsdorf et al. (2002),
# although those contributions aren't based on weights that sum to 1.

Pf <- fisher_index(p2, p1, q2, q1)
Pl <- laspeyres_index(p2, p1, q1)

(1 / (1 + Pf) * wl + Pl / (1 + Pf) * wp) * (p2 / p1 - 1)
#> [1]  0.01760668  0.18766299  0.05803833 -0.03510719  0.18397180 -0.01112258

# Also similar to the decomposition by Hallerbach (2005), noting that
# the Euler weights are close to unity.

Pp <- paasche_index(p2, p1, q2)

(0.5 * sqrt(Pp / Pl) * wl + 0.5 * sqrt(Pl / Pp) * wp) * (p2 / p1 - 1)
#> [1]  0.01781577  0.18809422  0.05791874 -0.03417605  0.18220228 -0.01089519
```
