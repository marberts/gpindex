# Index weights

Calculate weights for a variety of different price indexes.

## Usage

``` r
index_weights(
  type = c("Carli", "Jevons", "Coggeshall", "Dutot", "Laspeyres", "HybridLaspeyres",
    "LloydMoulton", "Palgrave", "Paasche", "HybridPaasche", "Drobisch", "Unnamed",
    "Tornqvist", "Walsh1", "Walsh2", "MarshallEdgeworth", "GearyKhamis", "Vartia1",
    "MontgomeryVartia", "Vartia2", "SatoVartia", "Theil", "Rao", "Lowe", "Young",
    "HybridCSWD")
)
```

## Arguments

- type:

  The name of the index. See details for the possible types of indexes.

## Value

A function of current and base period prices/quantities that calculates
the relevant weights.

## Details

The `index_weights()` function returns a function to calculate weights
for a variety of price indexes. Weights for the following types of
indexes can be calculated.

- Carli / Jevons / Coggeshall

- Dutot

- Laspeyres / Lloyd-Moulton

- Hybrid Laspeyres (for use in a harmonic mean)

- Paasche / Palgrave

- Hybrid Paasche (for use in an arithmetic mean)

- Törnqvist / Unnamed

- Drobisch

- Walsh-I (for an arithmetic Walsh index)

- Walsh-II (for a geometric Walsh index)

- Marshall-Edgeworth

- Geary-Khamis

- Montgomery-Vartia / Vartia-I

- Sato-Vartia / Vartia-II

- Theil

- Rao

- Lowe

- Young

- Hybrid-CSWD

The weights need not sum to 1, as this normalization isn't always
appropriate (i.e., for the Vartia-I weights).

## Note

Naming for the indexes and weights generally follows the CPI manual
(2020), Balk (2008), von der Lippe (2007), and Selvanathan and Rao
(1994). In several cases two or more names correspond to the same
weights (e.g., Paasche and Palgrave, or Sato-Vartia and Vartia-II). The
calculations are given in the examples.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

von der Lippe, P. (2007). *Index Theory and Price Statistics*. Peter
Lang.

Selvanathan, E. A. and Rao, D. S. P. (1994). *Index Numbers: A
Stochastic Approach*. MacMillan.

## See also

[`update_weights()`](https://marberts.github.io/gpindex/reference/factor_weights.md)
for price-updating weights.

[`quantity_index()`](https://marberts.github.io/gpindex/reference/quantity_index.md)
to remap the arguments in these functions for a quantity index.

Other price index functions:
[`geks()`](https://marberts.github.io/gpindex/reference/geks.md),
[`price_indexes`](https://marberts.github.io/gpindex/reference/price_indexes.md),
[`splice_index()`](https://marberts.github.io/gpindex/reference/splice_index.md)

## Examples

``` r
p1 <- price6[[2]]
p2 <- price6[[3]]
q1 <- quantity6[[2]]
q2 <- quantity6[[3]]
pb <- price6[[1]]
qb <- quantity6[[1]]

# Explicit calculation for each of the different weights
# Carli/Jevons/Coggeshall

all.equal(index_weights("Carli")(p2), rep(1, length(p1)))
#> [1] TRUE

# Dutot

all.equal(index_weights("Dutot")(p1), p1)
#> [1] TRUE

# Laspeyres / Lloyd-Moulton

all.equal(index_weights("Laspeyres")(p1, q1), p1 * q1)
#> [1] TRUE

# Hybrid Laspeyres

all.equal(index_weights("HybridLaspeyres")(p2, q1), p2 * q1)
#> [1] TRUE

# Paasche / Palgrave

all.equal(index_weights("Paasche")(p2, q2), p2 * q2)
#> [1] TRUE

# Hybrid Paasche

all.equal(index_weights("HybridPaasche")(p1, q2), p1 * q2)
#> [1] TRUE

# Tornqvist / Unnamed

all.equal(
  index_weights("Tornqvist")(p2, p1, q2, q1),
  0.5 * p1 * q1 / sum(p1 * q1) + 0.5 * p2 * q2 / sum(p2 * q2)
)
#> [1] TRUE

# Drobisch

all.equal(
  index_weights("Drobisch")(p2, p1, q2, q1),
  0.5 * p1 * q1 / sum(p1 * q1) + 0.5 * p1 * q2 / sum(p1 * q2)
)
#> [1] TRUE

# Walsh-I

all.equal(
  index_weights("Walsh1")(p1, q2, q1),
  p1 * sqrt(q1 * q2)
)
#> [1] TRUE

# Marshall-Edgeworth

all.equal(
  index_weights("MarshallEdgeworth")(p1, q2, q1),
  p1 * (q1 + q2)
)
#> [1] TRUE

# Geary-Khamis

all.equal(
  index_weights("GearyKhamis")(p1, q2, q1),
  p1 / (1 / q1 + 1 / q2)
)
#> [1] TRUE

# Montgomery-Vartia / Vartia-I

all.equal(
  index_weights("MontgomeryVartia")(p2, p1, q2, q1),
  logmean(p1 * q1, p2 * q2) / logmean(sum(p1 * q1), sum(p2 * q2))
)
#> [1] TRUE

# Sato-Vartia / Vartia-II

all.equal(
  index_weights("SatoVartia")(p2, p1, q2, q1),
  logmean(p1 * q1 / sum(p1 * q1), p2 * q2 / sum(p2 * q2))
)
#> [1] TRUE

# Walsh-II

all.equal(
  index_weights("Walsh2")(p2, p1, q2, q1),
  sqrt(p1 * q1 * p2 * q2)
)
#> [1] TRUE

# Theil

all.equal(index_weights("Theil")(p2, p1, q2, q1), {
  w0 <- scale_weights(p1 * q1)
  w1 <- scale_weights(p2 * q2)
  (w0 * w1 * (w0 + w1) / 2)^(1 / 3)
})
#> [1] TRUE

# Rao

all.equal(index_weights("Rao")(p2, p1, q2, q1), {
  w0 <- scale_weights(p1 * q1)
  w1 <- scale_weights(p2 * q2)
  w0 * w1 / (w0 + w1)
})
#> [1] TRUE

# Lowe

all.equal(index_weights("Lowe")(p1, qb), p1 * qb)
#> [1] TRUE

# Young

all.equal(index_weights("Young")(pb, qb), pb * qb)
#> [1] TRUE

# Hybrid CSWD (to approximate a CSWD index)

all.equal(index_weights("HybridCSWD")(p2, p1), sqrt(p1 / p2))
#> [1] TRUE
```
