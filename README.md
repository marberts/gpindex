
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Generalized price and quantity indexes

A small R package for calculating lots of different price indexes, and
by extension quantity indexes. Provides tools to build and work with any
type of generalized bilateral index (of which most price indexes are),
along with a few important indexes that don’t belong to the generalized
family.

## Installation

``` r
devtools::install_github("marberts/gpindex")
```

## Usage

``` r
library(gpindex)

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
     mean_arithmetic(t2 / t1, s1) * mean_arithmetic(t3 / t2, weights_factor(t2 / t1, s1, 1))
)
#> [1] 1.345

index_arithmetic(price6$t3, price6$t1, quantity6$t2, quantity6$t1, type = "Laspeyres")
#> [1] 1.345

# Get quote contributions for the Paasche index

with(price6, weights_h2a(t2 / t1, s2) * t2 / t1)
#> [1] 0.09411765 0.26470588 0.24215686 0.08921569 0.64509804 0.04705882

# Calculate a Fisher index over 5 periods

mapply(index_fisher, price6[-1], price6[1], quantity6[-1], quantity6[1])
#>       t2       t3       t4       t5 
#> 1.401050 1.272099 1.176163 1.071172

# Calculate a two-level index with different formulas

# Split data by even and odd rows
f <- rep(letters[1:2], 3)
prices <- split(price6[[2]] / price6[[1]], f)

# Odd groups get a weight of 30% and even group gets 70%
weight <- c(0.3, 0.7)

# Calculate lower-level indexes
(lower <- unlist(Map(mean_geometric, prices)))
#>        a        b 
#> 1.297431 1.188784

# Calculate upper-level index
(upper <- mean_arithmetic(lower, weight))
#> [1] 1.221378

# Calculate quote contributions for lower-level indexes
(con_lower <- Map("*", Map(weights_g2a, prices), prices))
#> $a
#> [1] 0.4156788 0.4327620 0.4489901
#> 
#> $b
#> [1] 0.5866980 0.2902257 0.3118606

# Calculate quote contributions for upper-level index
(con_upper <- unlist(Map("*", con_lower, weight)))
#>        a1        a2        a3        b1        b2        b3 
#> 0.1247036 0.1298286 0.1346970 0.4106886 0.2031580 0.2183024
```
