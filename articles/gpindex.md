# Generalized Price and Quantity Indexes

Generalized-mean price indexes are a large family of price indexes with
nice properties, such as the mean-value and identity properties (e.g.,
Balk, 2008, Chapter 3). When used with value-share weights, these
indexes satisfy the key homogeneity properties, commensurability, and
are consistent in aggregation. This last feature makes generalized-mean
indexes natural candidates for making national statistics, and this
justifies the hierarchical structure used by national statistical
agencies for calculating and disseminating collections of price indexes.

Almost all bilateral price indexes used in practice are either
generalized-mean indexes (like the Laspeyres and Paasche index) or are
nested generalized-mean indexes (like the Fisher index). The purpose of
this vignette is to show some of the key functions for working with
generalized-mean indexes. In what follows, everything is framed as a
price index to avoid duplication; it is trivial to turn a price index
into its analogous quantity index by simply switching prices and
quantities.

## Making generalized-mean indexes

A generalized-mean price index is a weighted generalized mean of price
relatives. Given a set of price relatives and weights, any
generalized-mean price index is easily calculated with the
[`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md)
function. What distinguishes different generalized-mean price indexes
are the weights and the order of the generalized mean. For example, the
standard Laspeyres index uses base-period value-share weights in a
generalized mean of order 1 (arithmetic mean).

``` r
library(gpindex)

# Start with some data on prices and quantities for 6 products
# over 5 periods
price6
#>   t1  t2  t3  t4  t5
#> 1  1 1.2 1.0 0.8 1.0
#> 2  1 3.0 1.0 0.5 1.0
#> 3  1 1.3 1.5 1.6 1.6
#> 4  1 0.7 0.5 0.3 0.1
#> 5  1 1.4 1.7 1.9 2.0
#> 6  1 0.8 0.6 0.4 0.2
quantity6
#>    t1  t2  t3  t4   t5
#> 1 1.0 0.8 1.0 1.2  0.9
#> 2 1.0 0.9 1.1 1.2  1.2
#> 3 2.0 1.9 1.8 1.9  2.0
#> 4 1.0 1.3 3.0 6.0 12.0
#> 5 4.5 4.7 5.0 5.6  6.5
#> 6 0.5 0.6 0.8 1.3  2.5

# We'll only need prices and quantities for a few periods
p1 <- price6[[1]]
p2 <- price6[[2]]
p2 <- price6[[3]]
q1 <- quantity6[[1]]
q2 <- quantity6[[2]]

s1 <- p1 * q1
s2 <- p1 * q2

# Laspeyres index
arithmetic_mean(p1 / p1, s1)
#> [1] 1
```

Changing the order of the generalized mean to \\1 - \sigma\\, where
\\\sigma\\ is an elasticity of substitution, gives a Lloyd-Moulton
index, whereas changing the weights to current-period value-shares gives
a Palgrave index. This is the essence of the atomistic approach in
chapter 2 of Selvanathan and Rao (1994).

``` r
# Lloyd-Moulton index (elasticity of substitution -1)
quadratic_mean <- generalized_mean(2)
quadratic_mean(p1 / p1, s1)
#> [1] 1

# Palgrave index
arithmetic_mean(p1 / p1, s2)
#> [1] 1
```

Generalized-mean indexes can also be nested together to get indexes like
the Fisher, Drobisch, or AG mean index. The
[`nested_mean()`](https://marberts.github.io/gpindex/reference/nested_mean.md)
function is a simple wrapper for
[`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md)
for these cases.

``` r
# Fisher index
fisher_mean(p1 / p1, s1, s2)
#> [1] 1

# Drobisch index
drobisch_mean <- nested_mean(1, c(1, 1))
drobisch_mean(p1 / p1, s1, s2)
#> [1] 1

# Geometric AG mean index (elasticity of substitution 0.25)
ag_mean <- nested_mean(0, c(0, 1), c(0.25, 0.75))
ag_mean(p1 / p1, s1, s1)
#> [1] 1
```

On top of these basic mathematical tools are functions for making
standard price indexes when both prices and quantities are known.
Weights for a large variety of indexes can be calculated with
[`index_weights()`](https://marberts.github.io/gpindex/reference/index_weights.md),
which can be plugged into the relevant generalized mean to calculate
most common price indexes, and many uncommon ones. The `price_index`
functions provide a simple wrapper, with the
[`quantity_index()`](https://marberts.github.io/gpindex/reference/quantity_index.md)
function turning each of these into its analogous quantity index.

``` r
# Laspeyres index, again
arithmetic_mean(p1 / p1, index_weights("Laspeyres")(p1, q1))
#> [1] 1

laspeyres_index(p1, p1, q1)
#> [1] 1
```

## Decomposing indexes

Two important functions for decomposing generalized means are given by
[`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
and
[`factor_weights()`](https://marberts.github.io/gpindex/reference/factor_weights.md).
These functions augment the weights in a generalized mean, and can be
used to calculate percent-change contributions (with, e.g.,
[`contributions()`](https://marberts.github.io/gpindex/reference/contributions.md))
and price-update weights for generalized-mean indexes.

``` r
quadratic_decomposition <- transmute_weights(2, 1)

arithmetic_mean(p1 / p1, quadratic_decomposition(p1 / p1, s1))
#> [1] 1
quadratic_mean(p1 / p1, s1)
#> [1] 1

quadratic_contributions <- contributions(2)
quadratic_contributions(p1 / p1, s1)
#> [1] 0 0 0 0 0 0

quadratic_update <- factor_weights(2)

quadratic_mean(p2 / p1, s1)
#> [1] 1.411914
quadratic_mean(p2 / p1, quadratic_update(p1 / p1, s1)) *
  quadratic_mean(p1 / p1, s1)
#> [1] 1.411914
```

Percent-change contributions can similarly be calculated for indexes
that nest generalized means.

``` r
ag_decomposition <- nested_transmute(0, c(0, 1), 1, c(0.25, 0.75))

ag_mean(p1 / p1, s1, s1)
#> [1] 1
arithmetic_mean(p1 / p1, ag_decomposition(p1 / p1, s1, s1))
#> [1] 1

ag_contributions <- nested_contributions(0, c(0, 1), c(0.25, 0.75))
ag_contributions(p1 / p1, s1, s1)
#> [1] 0 0 0 0 0 0
```

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

Selvanathan, E. A. and Rao, D. S. P. (1994). *Index Numbers: A
Stochastic Approach*. MacMillan.
