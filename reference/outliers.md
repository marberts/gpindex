# Outlier detection for price relatives

Standard cutoff-based methods for detecting outliers with price
relatives.

## Usage

``` r
quartile_method(x, cu = 2.5, cl = cu, a = 0, type = 7)

resistant_fences(x, cu = 2.5, cl = cu, a = 0, type = 7)

kimber_method(x, cu = 2.5, cl = cu, a = 0, type = 7)

robust_z(x, cu = 2.5, cl = cu)

fixed_cutoff(x, cu = 2.5, cl = 1/cu)

tukey_algorithm(x, cu = 2.5, cl = cu, type = 7)

hb_transform(x)
```

## Arguments

- x:

  A numeric vector, usually of price relatives. These can be made with,
  e.g.,
  [`back_period()`](https://marberts.github.io/gpindex/reference/back_period.md).

- cu, cl:

  A number giving the upper and lower cutoffs for each element of `x`.

- a:

  A number between 0 and 1 giving the scale factor for the median to
  establish the minimum dispersion between quartiles for each element of
  `x`. The default does not set a minimum dispersion.

- type:

  See [`quantile()`](https://rdrr.io/r/stats/quantile.html).

## Value

A logical vector, the same length as `x`, that is `TRUE` if the
corresponding element of `x` is identified as an outlier, `FALSE`
otherwise.

## Details

Each of these functions constructs an interval of the form \\\[b_l(x) -
c_l \times l(x), b_u(x) + c_u \times u(x)\]\\ and assigns a value in `x`
as `TRUE` if that value does not belong to the interval, `FALSE`
otherwise. The methods differ in how they construct the values
\\b_l(x)\\, \\b_u(x)\\, \\l(x)\\, and \\u(x)\\. Any missing values in
`x` are ignored when calculating the cutoffs, but will return `NA`.

The fixed cutoff method is the simplest, and just uses the interval
\\\[c_l, c_u\]\\.

The quartile method and Tukey algorithm are described in paragraphs
5.113 to 5.135 of the CPI manual (2020), as well as by Rais (2008) and
Hutton (2008). The resistant fences method is an alternative to the
quartile method, and is described by Rais (2008) and Hutton (2008). The
Kimber method is yet another alternative. Quantile-based methods often
identify price relatives as outliers because the distribution is
concentrated around 1; setting `a > 0` puts a floor on the minimum
dispersion between quantiles as a fraction of the median. See the
references for more details.

The robust Z-score is the usual method to identify relatives in the
(asymmetric) tails of the distribution, simply replacing the mean with
the median, and the standard deviation with the median absolute
deviation.

These methods often assume that price relatives are symmetrically
distributed (if not Gaussian). As the distribution of price relatives
often has a long right tail, the natural logarithm can be used to
transform price relative before identifying outliers (sometimes under
the assumption that price relatives are distributed log-normal). The
Hidiroglou-Berthelot transformation is another approach, described in
the CPI manual (par. 5.124). (Sometimes the transformed price relatives
are multiplied by \\\max(p_1, p_0)^u\\, for some \\0 \le u \le 1\\, so
that products with a larger price get flagged as outliers (par. 5.128).)

## References

Hutton, H. (2008). Dynamic outlier detection in price index surveys.
*Proceedings of the Survey Methods Section: Statistical Society of
Canada Annual Meeting*.

IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020). *Consumer Price
Index Manual: Concepts and Methods*. International Monetary Fund.

Rais, S. (2008). Outlier detection for the Consumer Price Index.
*Proceedings of the Survey Methods Section: Statistical Society of
Canada Annual Meeting*.

## See also

[`grouped()`](https://marberts.github.io/gpindex/reference/grouped.md)
to make each of these functions operate on grouped data.

[`back_period()`](https://marberts.github.io/gpindex/reference/back_period.md)/[`base_period()`](https://marberts.github.io/gpindex/reference/back_period.md)
for a simple utility function to turn prices in a table into price
relatives.

The `HBmethod()` function in the univOutl package for the
Hidiroglou-Berthelot method for identifying outliers.

## Examples

``` r
set.seed(1234)

x <- rlnorm(10)

fixed_cutoff(x)
#> Warning: this function is deprecated and will be removed; use 'outliers()' instead
#>  [1]  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
robust_z(x)
#> Warning: this function is deprecated and will be removed; use 'outliers()' instead
#>  [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
quartile_method(x)
#> Warning: this function is deprecated and will be removed; use 'outliers()' instead
#>  [1] FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
resistant_fences(x) # always identifies fewer outliers than above
#> Warning: this function is deprecated and will be removed; use 'outliers()' instead
#>  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
tukey_algorithm(x)
#> Warning: this function is deprecated and will be removed; use 'outliers()' instead
#>  [1] FALSE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE

log(x)
#>  [1] -1.2070657  0.2774292  1.0844412 -2.3456977  0.4291247  0.5060559
#>  [7] -0.5747400 -0.5466319 -0.5644520 -0.8900378
hb_transform(x)
#>  [1] -0.918538151  1.300051411  4.154877789 -4.990623335  1.676813038
#>  [6]  1.890871823 -0.019423964  0.008909836 -0.008989935 -0.397291327

# Works the same for grouped data.

f <- c("a", "b", "a", "a", "b", "b", "b", "a", "a", "b")
grouped(quartile_method)(x, group = f)
#> Warning: this function is deprecated and will be removed; use 'outliers()' instead
#> Warning: this function is deprecated and will be removed; use 'outliers()' instead
#>  [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```
