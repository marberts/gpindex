
# Generalized+ price and quantity indices

A small R package for calculating lots of different price and quantity
indices. Provides tools to build and work with any type of generalized
bilateral index (of which most price and quantity indices are), along
with a few important indices that don’t belong to the generalized
family.

## Installation

``` r
devtools::install_github("marberts/gpindex")
```

## Useage

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
     mean_arithmetic(t2 / t1, s1) * mean_arithmetic(t3 / t2, index_price_update(t2 / t1, s1))
)
#> [1] 1.345

index_arithmetic(price6$t3, price6$t1, quantity6$t2, quantity6$t1, type = "Laspeyres")
#> [1] 1.345

# Get quote contributions for the Paasche index

contribution_harmonic(price6$t2, price6$t1, quantity6$t2, quantity6$t1, type = "Paasche")
#> [1] 0.09411765 0.26470588 0.24215686 0.08921569 0.64509804 0.04705882

# Can also be done if only weights are known

with(price6, weights_h2a(t2 / t1, s2) * t2 / t1)
#> [1] 0.09411765 0.26470588 0.24215686 0.08921569 0.64509804 0.04705882

# Calculate a Fisher index and its quote contributions over 5 periods

mapply(index_fisher, price6[-1], price6[1], quantity6[-1], quantity6[1])
#>       t2       t3       t4       t5 
#> 1.401050 1.272099 1.176163 1.071172

mapply(contribution_fisher, price6[-1], price6[1], quantity6[-1], quantity6[1])
#>              t2         t3         t4         t5
#> [1,] 0.10697189 0.08907398 0.06705256 0.06321777
#> [2,] 0.28223440 0.09312065 0.04190785 0.07007160
#> [3,] 0.25101850 0.25508190 0.24331130 0.20960760
#> [4,] 0.07967238 0.08500374 0.06996278 0.03168096
#> [5,] 0.63759973 0.71581266 0.72845120 0.68090582
#> [6,] 0.04355312 0.03400621 0.02547776 0.01568868

# Calculate a two-level index with different formulas over 5 periods

# Split data by even and odd rows
f <- rep(letters[1:2], 3)
grouped_prices <- split(price6, f)

# Odd groups get a weight of 30% and even group gets 70%
w <- c(0.3, 0.7)

# Calculate group-level Jevons indices
eas <- sapply(grouped_prices, 
              function(df) mapply(index_geometric, df[-1], df[1], MoreArgs = list(type = "Jevons"))
              )

# Aggregate with an arithmetic index
apply(eas, 1, mean_arithmetic, w)
#>        t2        t3        t4        t5 
#> 1.2213783 0.8784622 0.6774778 0.6320930

# Calculate quote contributions for each group
contrib <- lapply(grouped_prices, 
                  function(df) as.data.frame(
                    mapply(contribution_geometric, df[-1], df[1], MoreArgs = list(type = "Jevons")),
                    rownames(df))
                  )
(contrib <- unsplit(lapply(seq_along(contrib), function(i) contrib[[i]] * w[i]), f))
#>          t2        t3         t4         t5
#> 1 0.1247036 0.1159168 0.10138853 0.11980391
#> 2 0.4106886 0.1882879 0.10260298 0.10537660
#> 3 0.1298286 0.1424933 0.14481984 0.15244924
#> 4 0.2031580 0.1335605 0.07943958 0.03429205
#> 5 0.1346970 0.1514491 0.15722867 0.16983063
#> 6 0.2183024 0.1467546 0.09199817 0.05034058

# Adds up to the same value as the index
colSums(contrib)
#>        t2        t3        t4        t5 
#> 1.2213783 0.8784622 0.6774778 0.6320930
```
