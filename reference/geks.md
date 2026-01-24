# GEKS index

Calculate a generalized inter-temporal GEKS price index over a rolling
window.

## Usage

``` r
geks(f, r = 0)

tornqvist_geks(
  p,
  q,
  period,
  product,
  window = nlevels(period),
  n = window - 1L,
  na.rm = FALSE,
  match_method = c("all", "back-price")
)

fisher_geks(
  p,
  q,
  period,
  product,
  window = nlevels(period),
  n = window - 1L,
  na.rm = FALSE,
  match_method = c("all", "back-price")
)

walsh_geks(
  p,
  q,
  period,
  product,
  window = nlevels(period),
  n = window - 1L,
  na.rm = FALSE,
  match_method = c("all", "back-price")
)
```

## Arguments

- f:

  A [price index
  function](https://marberts.github.io/gpindex/reference/price_indexes.md)
  that uses information on both base and current-period prices and
  quantities, and satisfies the time-reversal test. Usually a Törnqvist,
  Fisher, or Walsh index.

- r:

  A finite number giving the order of the generalized mean used to
  average price indexes over the rolling window. The default uses a
  geometric mean.

- p:

  A numeric vector of prices, the same length as `q`.

- q:

  A numeric vector of quantities, the same length as `p`.

- period:

  A factor, or something that can be coerced into one, that gives the
  corresponding time period for each element in `p` and `q`. The
  ordering of time periods follows the levels of `period` to agree with
  [`cut()`](https://rdrr.io/r/base/cut.POSIXt.html).

- product:

  A factor, or something that can be coerced into one, that gives the
  corresponding product identifier for each element in `p` and `q`.

- window:

  A positive integer giving the length of the rolling window. The
  default is a window that encompasses all periods in `period`.
  Non-integers are truncated towards zero.

- n:

  A positive integer giving the length of the index series for each
  window, starting from the end of the window. For example, if there are
  13 periods in `window`, setting `n = 1` gives the index for period 13.
  The default gives an index for each period in `window`. Non-integers
  are truncated towards zero.

- na.rm:

  Passed to `f` to control if missing values are removed.

- match_method:

  Either 'all' to match all products against each other (the default) or
  'back-price' to match only back prices. The later can be faster when
  there is lots of product imbalanced, but should be used with a
  balanced index-number formula `f`.

## Value

`geks()` returns a function:

    function(p,
             q,
             period,
             product,
             window = nlevels(period),
             n = window - 1,
             na.rm = FALSE,
             match_method = c("all", "back-price")){...}

This calculates a period-over-period GEKS index with the desired
index-number formula, returning a list for each window with a
named-numeric vector of index values.

`tornqvist_geks()`, `fisher_geks()`, and `walsh_geks()` each return a
list with a named numeric vector giving the value of the respective
period-over-period GEKS index for each window.

## Note

Like
[`back_period()`](https://marberts.github.io/gpindex/reference/back_period.md),
if multiple prices correspond to a period-product pair, then the back
price at a point in time is always the first price for that product in
the previous period. Unlike a bilateral index, however, duplicated
period-product pairs can have more subtle implications for a
multilateral index.

## References

Balk, B. M. (2008). *Price and Quantity Index Numbers*. Cambridge
University Press.

IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

Ivancic, L., Diewert, W. E., and Fox, K. J. (2011). Scanner data, time
aggregation and the construction of price indexes. *Journal of
Econometrics*, 161(1): 24–35.

## See also

`GEKSIndex()` in the IndexNumR package for an implementation of the GEKS
index with more options.

Other price index functions:
[`index_weights()`](https://marberts.github.io/gpindex/reference/index_weights.md),
[`price_indexes`](https://marberts.github.io/gpindex/reference/price_indexes.md),
[`splice_index()`](https://marberts.github.io/gpindex/reference/splice_index.md)

## Examples

``` r
price <- 1:10
quantity <- 10:1
period <- rep(1:5, 2)
product <- rep(letters[1:2], each = 5)

cumprod(tornqvist_geks(price, quantity, period, product)[[1]])
#>        2        3        4        5 
#> 1.413257 1.835676 2.284565 2.789856 

# Calculate the index over a rolling window.

(tg <- tornqvist_geks(price, quantity, period, product, window = 3))
#> [[1]]
#>        2        3 
#> 1.391443 1.294442 
#> 
#> [[2]]
#>        3        4 
#> 1.292486 1.238393 
#> 
#> [[3]]
#>        4        5 
#> 1.238417 1.205921 
#> 

# Use a movement splice to combine the indexes in each window.

splice_index(tg, 2)
#>        2        3        4        5 
#> 1.391443 1.801142 2.230521 2.689833 

# ... or use a mean splice.

splice_index(tg)
#>        2        3        4        5 
#> 1.391443 1.801142 2.228836 2.687826 

# Use all non-missing data.

quantity[2] <- NA
fisher_geks(price, quantity, period, product, na.rm = TRUE)
#> [[1]]
#>        2        3        4        5 
#> 1.438137 1.234230 1.234212 1.216746 
#> 

# Remove records with any missing data.

fg <- geks(balanced(fisher_index))
fg(price, quantity, period, product, na.rm = TRUE)
#> [[1]]
#>        2        3        4        5 
#> 1.501481 1.148250 1.219688 1.199513 
#> 

# Make a Jevons GEKS index.

jevons_geks <- geks(\(p1, p0, ..., na.rm) jevons_index(p1, p0, na.rm))
jevons_geks(price, quantity, period, product)
#> [[1]]
#>        2        3        4        5 
#> 1.527525 1.309307 1.224745 1.178511 
#> 
```
