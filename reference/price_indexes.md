# Price indexes

Calculate a variety of price indexes using information on prices and
quantities at two points in time.

## Usage

``` r
arithmetic_index(type)

geometric_index(type)

harmonic_index(type)

laspeyres_index(p1, p0, q0, na.rm = FALSE)

paasche_index(p1, p0, q1, na.rm = FALSE)

jevons_index(p1, p0, na.rm = FALSE)

lowe_index(p1, p0, qb, na.rm = FALSE)

young_index(p1, p0, pb, qb, na.rm = FALSE)

fisher_index(p1, p0, q1, q0, na.rm = FALSE)

hlp_index(p1, p0, q1, q0, na.rm = FALSE)

lm_index(elasticity)

cswd_index(p1, p0, na.rm = FALSE)

cswdb_index(p1, p0, q1, q0, na.rm = FALSE)

bw_index(p1, p0, na.rm = FALSE)

stuvel_index(a, b)

arithmetic_agmean_index(elasticity)

geometric_agmean_index(elasticity)

lehr_index(p1, p0, q1, q0, na.rm = FALSE)

martini_index(a)
```

## Arguments

- type:

  The name of the index. See details for the possible types of indexes.

- p1:

  Current-period prices.

- p0:

  Base-period prices.

- q0:

  Base-period quantities.

- na.rm:

  Should missing values be removed? By default missing values for prices
  or quantities return a missing value.

- q1:

  Current-period quantities.

- qb:

  Period-b quantities for the Lowe/Young index.

- pb:

  Period-b prices for the Lowe/Young index.

- elasticity:

  The elasticity of substitution for the Lloyd-Moulton and AG mean
  indexes.

- a, b:

  Parameters for the generalized Stuvel index or Martini index.

## Value

`arithmetic_index()`, `geometric_index()`, `harmonic_index()`, and
`stuvel_index()` each return a function to compute the relevant price
indexes; `lm_index()`, `arithmetic_agmean_index()`, and
`geometric_agmean_index()` each return a function to calculate the
relevant index for a given elasticity of substitution. The others return
a numeric value giving the change in price between the base period and
current period.

## Details

The `arithmetic_index()`, `geometric_index()`, and `harmonic_index()`
functions return a function to calculate a given type of arithmetic,
geometric (logarithmic), and harmonic index. Together, these functions
produce functions to calculate the following indexes.

- **Arithmetic indexes**

- Carli

- Dutot

- Laspeyres

- Palgrave

- Unnamed index (arithmetic mean of Laspeyres and Palgrave)

- Drobisch (or Sidgwick, arithmetic mean of Laspeyres and Paasche)

- Walsh-I (arithmetic Walsh)

- Marshall-Edgeworth

- Geary-Khamis

- Lowe

- Young

- Hybrid-CSWD

- **Geometric indexes**

- Jevons

- Geometric Laspeyres (or Jöhr)

- Geometric Paasche

- Geometric Young

- Törnqvist (or Törnqvist-Theil)

- Montgomery-Vartia / Vartia-I

- Sato-Vartia / Vartia-II

- Walsh-II (geometric Walsh)

- Theil

- Rao

- **Harmonic indexes**

- Coggeshall (equally weighted harmonic index)

- Paasche

- Harmonic Laspeyres

- Harmonic Young

Along with the `lm_index()` function to calculate the Lloyd-Moulton
index, these are just convenient wrappers for
[`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md)
and
[`index_weights()`](https://marberts.github.io/gpindex/reference/index_weights.md).

The Laspeyres, Paasche, Jevons, Lowe, and Young indexes are among the
most common price indexes, and so they get their own functions. The
`laspeyres_index()`, `lowe_index()`, and `young_index()` functions
correspond to setting the appropriate `type` in `arithmetic_index()`;
`paasche_index()` and `jevons_index()` instead come from the
`harmonic_index()` and `geometric_index()` functions.

In addition to these indexes, there are also functions for calculating a
variety of indexes based on nested generalized means. The Fisher index
is the geometric mean of the arithmetic Laspeyres and Paasche indexes;
the Harmonic Laspeyres Paasche (or Harmonic Paasche Laspeyres) index is
the harmonic analog of the Fisher index (8054 on Fisher's list). The
Carruthers-Sellwood-Ward-Dalen and Carruthers-Sellwood-Ward-Dalen-Balk
indexes are sample analogs of the Fisher index; the Balk-Walsh index is
the sample analog of the Walsh index. The AG mean index is the
arithmetic or geometric mean of the geometric and arithmetic Laspeyres
indexes, weighted by the elasticity of substitution. The
`stuvel_index()` function returns a function to calculate a Stuvel index
of the given parameters. The Lehr index is an alternative to the
Geary-Khamis index, and is the implicit price index for Fisher's index
4153. The Martini index is a Lowe index where the quantities are the
weighted geometric average of current and base period quantities.

## Note

There are different ways to deal with missing values in a price index,
and care should be taken when relying on these functions to remove
missing values. Setting `na.rm = TRUE` removes price relatives with
missing information, either because of a missing price or a missing
weight, while using all available non-missing information to make the
weights.

Certain properties of an index-number formula may not work as expected
when removing missing values if there is ambiguity about how to remove
missing values from the weights (as in, e.g., a Törnqvist or Sato-Vartia
index). The
[`balanced()`](https://marberts.github.io/gpindex/reference/balanced.md)
operator may be helpful, as it balances the removal of missing values
across prices and quantities prior to making the weights.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

Fisher, I. (1922). *The Making of Index Numbers*. Houghton Mifflin
Company.

IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

von der Lippe, P. (2007). *Index Theory and Price Statistics*. Peter
Lang.

Selvanathan, E. A. and Rao, D. S. P. (1994). *Index Numbers: A
Stochastic Approach*. MacMillan.

## See also

[`generalized_mean()`](https://marberts.github.io/gpindex/reference/generalized_mean.md)
for the generalized mean that powers most of these functions.

[`contributions()`](https://marberts.github.io/gpindex/reference/contributions.md)
for calculating percent-change contributions.

[`quantity_index()`](https://marberts.github.io/gpindex/reference/quantity_index.md)
to remap the arguments in these functions for a quantity index.

[`price6()`](https://marberts.github.io/gpindex/reference/price_data.md)
for an example of how to use these functions with more than two time
periods.

The piar package has more functionality working with price indexes for
multiple groups of products over many time periods.

Other price index functions:
[`geks()`](https://marberts.github.io/gpindex/reference/geks.md),
[`index_weights()`](https://marberts.github.io/gpindex/reference/index_weights.md),
[`splice_index()`](https://marberts.github.io/gpindex/reference/splice_index.md)

## Examples

``` r
p1 <- price6[[2]]
p2 <- price6[[3]]
q1 <- quantity6[[2]]
q2 <- quantity6[[3]]

# Most indexes can be calculated by combining the appropriate weights
# with the correct type of mean.

laspeyres_index(p2, p1, q1)
#> [1] 0.9609929
arithmetic_mean(p2 / p1, index_weights("Laspeyres")(p1, q1))
#> [1] 0.9609929

geometric_index("Laspeyres")(p2, p1, q1)
#> [1] 0.8705581
geometric_mean(p2 / p1, index_weights("Laspeyres")(p1, q1))
#> [1] 0.8705581

# NAs get special treatment.

p_na <- replace(p1, 6, NA)

laspeyres_index(p2, p_na, q1, na.rm = TRUE) # drops the last price relative
#> [1] 0.9684288

sum(p2 * q1, na.rm = TRUE) /
  sum(p_na * q1, na.rm = TRUE) # drops the last period-0 price
#> [1] 0.9948605

# von Bortkiewicz decomposition

paasche_index(p2, p1, q2) / laspeyres_index(p2, p1, q1) - 1
#> [1] -0.04099992

wl <- scale_weights(index_weights("Laspeyres")(p1, q1))
pl <- laspeyres_index(p2, p1, q1)
ql <- quantity_index(laspeyres_index)(q2, q1, p1)

sum(wl * (p2 / p1 / pl - 1) * (q2 / q1 / ql - 1))
#> [1] -0.04099992

# Similar decomposition for geometric Laspeyres/Paasche.

wp <- scale_weights(index_weights("Paasche")(p2, q2))
gl <- geometric_index("Laspeyres")(p2, p1, q1)
gp <- geometric_index("Paasche")(p2, p1, q2)

log(gp / gl)
#> [1] 0.1388242

sum(scale_weights(wl) * (wp / wl - 1) * log(p2 / p1 / gl))
#> [1] 0.1388242
```
