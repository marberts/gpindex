
<!-- README.md is generated from README.Rmd. Please edit that file. -->

# Generalized Price and Quantity Indexes

<!-- Badges -->

[![CRAN
status](https://www.r-pkg.org/badges/version/gpindex)](https://cran.r-project.org/package=gpindex)
[![R-CMD-check](https://github.com/marberts/gpindex/workflows/R-CMD-check/badge.svg)](https://github.com/marberts/gpindex/actions)
[![codecov](https://codecov.io/gh/marberts/gpindex/branch/master/graph/badge.svg?token=lHDHsGHsLd)](https://app.codecov.io/gh/marberts/gpindex)

A small package for calculating lots of different price indexes, and by
extension quantity indexes. Provides tools to build and work with any
type of bilateral generalized-mean index (of which most price indexes
are), along with a few important indexes that don’t belong to the
generalized-mean family (e.g., superlative quadratic-mean indexes,
GEKS). Implements and extends many of the methods in Balk (2008), von
der Lippe (2001), and the CPI manual (2020) for bilateral price indexes.

## Installation

``` r
install.packages("gpindex")
```

The development version can be installed from GitHub.

``` r
devtools::install_github("marberts/gpindex")
```

## Usage

The examples here give a brief introduction to how to use the
functionality in this package. The package-level help page
(`package?gpindex`) gives much more detail.

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
p0 <- price6[[1]]; p1 <- price6[[2]]; p2 <- price6[[3]]
q0 <- price6[[1]]; q1 <- price6[[2]]

# There are functions to calculate all common price indexes,
# like the Laspeyres and Paasche index
laspeyres_index(p1, p0, q0)
#> [1] 1.4
paasche_index(p1, p0, q1)
#> [1] 1.811905

# The underlying mean functions are also available, as usually
# only price relatives and weights are known
s0 <- p0 * q0; s1 <- p1 * q1

arithmetic_mean(p1 / p0, s0)
#> [1] 1.4
harmonic_mean(p1 / p0, s1)
#> [1] 1.811905

# The mean representation of a Laspeyres index makes it easy to
# chain by price-updating the weights
laspeyres_index(p2, p0, q0)
#> [1] 1.05

arithmetic_mean(p1 / p0, s0) * 
  arithmetic_mean(p2 / p1, update_weights(p1 / p0, s0))
#> [1] 1.05

# The mean representation of a Paasche index makes it easy to
# calculate quote contributions
harmonic_contributions(p1 / p0, s1)
#> [1]  0.02857143  0.71428571  0.04642857 -0.02500000  0.06666667 -0.01904762

# The ideas are the same for more exotic indexes, 
# like the Lloyd-Moulton index

# Let's start by making some functions for the Lloyd-Moulton index
# when the elasticity of substitution is -1 (an output index)
lm <- lm_index(-1)
quadratic_mean <- generalized_mean(2)
quadratic_update <- factor_weights(2)
quadratic_contributions <- contributions(2)

# This index can be calculated as a mean of price relatives
lm(p1, p0, q0) 
#> [1] 1.592692
quadratic_mean(p1 / p0, s0)
#> [1] 1.592692

# Chained over time
lm(p2, p0, q0)
#> [1] 1.136515
quadratic_mean(p1 / p0, s0) * 
  quadratic_mean(p2 / p1, quadratic_update(p1 / p0, s0))
#> [1] 1.136515

# And decomposed to get the contributions of each relative
quadratic_contributions(p1 / p0, s0)
#> [1]  0.03110568  0.51154526  0.04832926 -0.03830484  0.06666667 -0.02665039
```

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). *Consumer Price
Index Manual: Theory and Practice*. International Monetary Fund.

von der Lippe, P. (2001). *Chain Indices: A Study in Price Index
Theory*, Spectrum of Federal Statistics vol. 16. Federal Statistical
Office, Wiesbaden.
