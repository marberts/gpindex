
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Generalized price and quantity indexes

<!-- Badges -->

[![CRAN
status](https://www.r-pkg.org/badges/version/gpindex)](https://cran.r-project.org/package=gpindex)

A small R package for calculating lots of different price indexes, and
by extension quantity indexes. Provides tools to build and work with any
type of generalized bilateral index (of which most price indexes are),
along with a few important indexes that don’t belong to the generalized
family.

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

# Calculate a Laspeyres and Paasche index
index_arithmetic(price6$t2, price6$t1, quantity6$t2, quantity6$t1, type = "Laspeyres")
#> [1] 1.42
index_harmonic(price6$t2, price6$t1, quantity6$t2, quantity6$t1, type = "Paasche")
#> [1] 1.382353

# Can also be done if only weights are available
s1 <- index_weights(price6$t2, price6$t1, quantity6$t2, quantity6$t1, "Laspeyres")
s2 <- index_weights(price6$t2, price6$t1, quantity6$t2, quantity6$t1, "Paasche")

with(price6, mean_arithmetic(t2 / t1, s1))
#> [1] 1.42
with(price6, mean_harmonic(t2 / t1, s2))
#> [1] 1.382353

# Chain the Laspeyres index by price-updating the weights
with(price6, 
     mean_arithmetic(t2 / t1, s1) * mean_arithmetic(t3 / t2, weights_update(t2 / t1, s1))
)
#> [1] 1.345

index_arithmetic(price6$t3, price6$t1, quantity6$t2, quantity6$t1, type = "Laspeyres")
#> [1] 1.345

# Get quote contributions for the Paasche index
with(price6, weights_h2a(t2 / t1, s2) * (t2 / t1 - 1))
#> [1]  0.01568627  0.17647059  0.05588235 -0.03823529  0.18431373 -0.01176471

# Calculate a Fisher index over 5 periods
mapply(index_fisher, price6[-1], price6[1], quantity6[-1], quantity6[1])
#>       t2       t3       t4       t5 
#> 1.401050 1.272099 1.176163 1.071172

# Calculate a two-level index with different formulas
# Split data by even and odd rows
f <- rep(c("even", "odd"), 3)
prices <- split(price6[[2]] / price6[[1]], f)

# Odd groups get a weight of 30% and even group gets 70%
weight <- c(0.3, 0.7)

# Calculate lower-level indexes
(lower <- unlist(Map(mean_geometric, prices)))
#>     even      odd 
#> 1.297431 1.188784

# Calculate upper-level index
(upper <- mean_arithmetic(lower, weight))
#> [1] 1.221378

# Calculate quote contributions for lower-level indexes
(con_lower <- Map("*", Map(weights_g2a, prices), Map("-", prices, 1)))
#> $even
#> [1] 0.06927979 0.09986815 0.12828288
#> 
#> $odd
#> [1]  0.39113201 -0.12438246 -0.07796516

# Calculate quote contributions for upper-level index
(con_upper <- unlist(Map("*", con_lower, weight)))
#>       even1       even2       even3        odd1        odd2        odd3 
#>  0.02078394  0.02996044  0.03848486  0.27379241 -0.08706772 -0.05457561
```
