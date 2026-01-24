# Sample price/quantity data

Prices and quantities for six products over five periods.

## Format

Each data frame has 6 rows and 5 columns, with each row corresponding to
a product and each column corresponding to a time period.

## Source

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

ILO, IMF, OECD, UNECE, and World Bank. (2004). *Producer Price Index
Manual: Theory and Practice*. International Monetary Fund.

## Note

Adapted from tables 3.1 and 3.2 in Balk (2008), which were adapted from
tables 19.1 and 19.2 in the PPI manual.

## Examples

``` r
# Recreate tables 3.4, 3.6, and 3.12 from Balk (2008).

index_formulas <- function(p1, p0, q1, q0) {
  c(
    harmonic_laspeyres = harmonic_index("Laspeyres")(p1, p0, q0),
    geometric_laspeyres = geometric_index("Laspeyres")(p1, p0, q0),
    laspeyres = arithmetic_index("Laspeyres")(p1, p0, q0),
    paasche = harmonic_index("Paasche")(p1, p0, q1),
    geometric_paasche = geometric_index("Paasche")(p1, p0, q1),
    palgrave = arithmetic_index("Palgrave")(p1, p0, q1),
    fisher = fisher_index(p1, p0, q1, q0),
    tornqvist = geometric_index("Tornqvist")(p1, p0, q1, q0),
    marshall_edgeworth =
        arithmetic_index("MarshallEdgeworth")(p1, p0, q1, q0),
    walsh1 = arithmetic_index("Walsh1")(p1, p0, q1, q0),
    vartia2 = geometric_index("Vartia2")(p1, p0, q1, q0),
    vartia1 = geometric_index("Vartia1")(p1, p0, q1, q0),
    stuvel = stuvel_index(2, 2)(p1, p0, q1, q0)
  )
}

res <- t(mapply(index_formulas, price6, price6[1], quantity6, quantity6[1]))
round(res, 4)
#>    harmonic_laspeyres geometric_laspeyres laspeyres paasche geometric_paasche
#> t1             1.0000              1.0000     1.000  1.0000            1.0000
#> t2             1.2542              1.3300     1.420  1.3824            1.4846
#> t3             1.1346              1.2523     1.345  1.2031            1.3268
#> t4             0.8732              1.1331     1.355  1.0209            1.3282
#> t5             0.5556              1.0999     1.440  0.7968            1.4153
#>    palgrave fisher tornqvist marshall_edgeworth walsh1 vartia2 vartia1 stuvel
#> t1   1.0000 1.0000    1.0000             1.0000 1.0000  1.0000  1.0000 1.0000
#> t2   1.6096 1.4011    1.4052             1.4010 1.4017  1.4018  1.4024 1.4042
#> t3   1.4161 1.2721    1.2890             1.2656 1.2850  1.2897  1.2907 1.2742
#> t4   1.5317 1.1762    1.2268             1.1438 1.2193  1.2335  1.2392 1.1551
#> t5   1.6720 1.0712    1.2477             0.9801 1.1850  1.2540  1.2678 0.9770
```
