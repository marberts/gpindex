
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Generalized Price and Quantity Indexes

<!-- Badges -->

[![CRAN
status](https://www.r-pkg.org/badges/version/gpindex)](https://cran.r-project.org/package=gpindex)
[![R-CMD-check](https://github.com/marberts/gpindex/workflows/R-CMD-check/badge.svg)](https://github.com/marberts/gpindex/actions)
[![codecov](https://codecov.io/gh/marberts/gpindex/branch/master/graph/badge.svg?token=lHDHsGHsLd)](https://codecov.io/gh/marberts/gpindex)

A small R package for calculating lots of different price indexes, and
by extension quantity indexes. Provides tools to build and work with any
type of bilateral generalized-mean index (of which most price indexes
are), along with a few important indexes that don’t belong to the
generalized-mean family.

## Installation

``` r
install.packages("gpindex")
```

The development version can be installed from GitHub.

``` r
devtools::install_github("marberts/gpindex")
```

## Usage

``` r
library(gpindex)

# Start with some data on prices and quantities
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

p0 <- price6[[1]]
p1 <- price6[[2]]
p2 <- price6[[3]]
q0 <- price6[[1]]
q1 <- price6[[2]]

# Calculate a Laspeyres and Paasche index
laspeyres_index(p1, p0, q0)
#> [1] 1.4
paasche_index(p1, p0, q1)
#> [1] 1.811905

# Can also be done if only weights are available
s0 <- index_weights("Laspeyres")(p0, q0)
s1 <- index_weights("Paasche")(p1, q1)

arithmetic_mean(p1 / p0, s0)
#> [1] 1.4
harmonic_mean(p1 / p0, s1)
#> [1] 1.811905

# Chain the Laspeyres index by price-updating the weights
arithmetic_mean(p1 / p0, s0) * arithmetic_mean(p2 / p1, update_weights(p1 / p0, s0))
#> [1] 1.05

laspeyres_index(p2, p0, q0)
#> [1] 1.05

# Get quote contributions for the Paasche index
harmonic_contributions(p1 / p0, s1)
#> [1]  0.02857143  0.71428571  0.04642857 -0.02500000  0.06666667 -0.01904762

# Also works for more exotic indexes, like the Lloyd-Moulton index
lm_index(p1, p0, q0, 0.5) # elasticity of 0.5
#> [1] 1.315599
generalized_mean(0.5)(p1 / p0, s0)
#> [1] 1.315599
generalized_mean(0.5)(p1 / p0, s0) * generalized_mean(0.5)(p2 / p1, factor_weights(0.5)(p1 / p0, s0))
#> [1] 1.003433
lm_index(p2, p0, q0, 0.5)
#> [1] 1.003433
contributions(0.5)(p1 / p0, s0)
#> [1]  0.03361012  0.26178360  0.04942922 -0.05699229  0.06468830 -0.03691970

# Calculate a Fisher index over 5 periods
mapply(fisher_index, price6, price6[1], quantity6, quantity6[1])
#>       t1       t2       t3       t4       t5 
#> 1.000000 1.401050 1.272099 1.176163 1.071172

# Calculate a two-level index with different formulas
# Split data by even and odd rows
(prices <- split(p1 / p0, c("even", "odd")))
#> $even
#> [1] 1.2 1.3 1.4
#> 
#> $odd
#> [1] 3.0 0.7 0.8

# Odd groups get a weight of 30% and even group gets 70%
weight <- c(0.3, 0.7)

# Calculate lower-level indexes
(lower <- sapply(prices, geometric_mean))
#>     even      odd 
#> 1.297431 1.188784

# Calculate upper-level index
arithmetic_mean(lower, weight)
#> [1] 1.221378

# Calculate quote contributions for lower-level indexes
(con_lower <- lapply(prices, geometric_contributions))
#> $even
#> [1] 0.06927979 0.09986815 0.12828288
#> 
#> $odd
#> [1]  0.39113201 -0.12438246 -0.07796516

# Calculate quote contributions for upper-level index
mapply("*", con_lower, weight)
#>            even         odd
#> [1,] 0.02078394  0.27379241
#> [2,] 0.02996044 -0.08706772
#> [3,] 0.03848486 -0.05457561
```
