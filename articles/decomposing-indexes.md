# Decomposing price indexes

*This vignette presents an extended version of Martin
([2021](#ref-martin2021)), and shows how to use the tools in this
package to make these decompositions.*

It is often useful to be able to decompose a price index into an
additive or multiplicative form to evaluate how each input to the index
affects its value. Following Balk ([2008](#ref-balk2008)), a price index
is said to admit an additive decomposition if there exist weights that
allow it to be represented as an arithmetic mean of price relatives, and
is said to admit a multiplicative decomposition if there are weights
that allow it to be represented as a geometric mean. Switching prices
for quantities gives the analogous statements for a quantity index, and
nothing is lost by focusing on price indexes.

There are several well-known decompositions for the most common types of
bilateral price indexes. Balk ([2008](#ref-balk2008), equation 4.13)
gives an additive decomposition for any index based on the geometric
mean by transmuting the weights in the geometric mean with the
logarithmic mean. This is the same decomposition derived by Reinsdorf,
Diewert, and Ehemann ([2002](#ref-reinsdorf2002), equation 20) for the
Törnqvist index. A similar approach yields a multiplicative
decomposition for any index based on the arithmetic mean, again using
the logarithmic mean ([Balk 2008](#ref-balk2008), equation 4.8).
Combining these results gives additive and multiplicative decompositions
for the Fisher index ([Reinsdorf, Diewert, and Ehemann 2002, sec.
6](#ref-reinsdorf2002)). The van IJzeren additive decomposition for the
Fisher index ([Balk 2008](#ref-balk2008), equation 4.18) is an
alternative that does not explicitly use the logarithmic mean. Each of
these decompositions results in weights that are positive and sum to
one, as required to represent an index as an arithmetic or geometric
mean.[¹](#fn1)

I show how the additive and multiplicative decompositions for geometric,
arithmetic, and Fisher indexes that use the logarithmic mean can be
consolidated and made more general by switching out the logarithmic mean
for the more general extended mean. The main result is a function that
transmutes the weights in a generalized mean of a given order so that it
can be represented as a generalized mean of any other order. This covers
additive and multiplicative decompositions for indexes that do not
belong to the arithmetic or geometric families, like harmonic indexes or
the Lloyd-Moulton index, and allows both additive and multiplicative
decompositions to be covered by a single equation, rather than treating
them as different cases. Expressing a generalized index as a generalized
mean of any other order also allows for the decomposition of indexes
that are nested generalized means, like the family of superlative
quadratic mean indexes that includes the Fisher index or the AG mean
index by Lent and Dorfman ([2009](#ref-lent2009)). I demonstrate some
useful properties of this general decomposition and show how it can be
used to decompose indexes based on other types on means and decompose
deflating a change in aggregate values with a price index. Throughout I
show how the tools in the *gpindex* package can be used to decompose a
price index.

## Decomposing generalized-mean indexes

A natural extension to the decompositions for indexes based on the
arithmetic and geometric means is to derive weights that transform an
index based on a generalized mean of order \\\rho\\ into one based on a
generalized mean of order \\\varsigma\\. To fix notation, let
\\\mathbf{r} = (r\_{1}, r\_{2}, \ldots, r\_{n}) \in
\mathbb{R}^{n}\_{++}\\ be a vector of price relatives for \\n\geq2\\
products and let \\\mathbf{w} = (w\_{1}, w\_{2}, \ldots, w\_{n}) \in
\Delta^{n - 1}\\ be the corresponding weights, where \\\Delta^{n - 1} =
\\\mathbf{w} \in \mathbb{R}\_{+}^{n} \| \sum\_{i = 1}^{n} w\_{i} = 1\\\\
is the unit simplex. The goal is to find a vector-valued function \\
\mathbf{v}(\mathbf{r}, \mathbf{w}; \rho, \varsigma) =
(v\_{1}(\mathbf{r}, \mathbf{w}; \rho, \varsigma), v\_{2}(\mathbf{r},
\mathbf{w}; \rho, \varsigma),\ldots, v\_{n}(\mathbf{r}, \mathbf{w};
\rho, \varsigma)) \\ mapping into \\\Delta^{n - 1}\\ such that \\
\mathfrak{M}\_{\rho}(\mathbf{r}, \mathbf{w}) \equiv
\mathfrak{M}\_{\varsigma}(\mathbf{r}, \mathbf{v}(\mathbf{r}, \mathbf{w};
\rho, \varsigma)), \\ where \\\mathfrak{M}\_{\rho}\\ is the generalized
mean \\ \mathfrak{M}\_{\rho}(\mathbf{r}, \mathbf{w}) = \begin{cases}
\left(\sum\_{i = 1}^{n} w\_{i} r_i^{\rho}\right)^{1 / \rho} & \text{if }
\rho \neq 0 \\ \prod\_{i = 1}^{n} r\_{i}^{w\_{i}} & \text{if } \rho = 0.
\end{cases} \\

Setting \\\varsigma = 1\\ then yields an additive decomposition for any
index based on a generalized mean of order \\\rho\\, such that \\
\mathfrak{M}\_{\rho}(\mathbf{r}, \mathbf{w}) = \sum\_{i=1}^{n} r\_{i}
v_i(\mathbf{r}, \mathbf{w}; \rho, 1), \\ and setting \\\varsigma = 0\\
yields a multiplicative decomposition, such that \\
\mathfrak{M}\_{\rho}(\mathbf{r}, \mathbf{w}) = \prod\_{i=1}^{n} r\_{i}^{
v_i(\mathbf{r}, \mathbf{w}; \rho, 0)}. \\ Note that any index that
admits an additive decomposition can be used to derive percent-change
contributions for each price relative, \\v_i(\mathbf{r}, \mathbf{w};
\rho, 1) (r_i - 1)\\, that sum up to \\\mathfrak{M}\_{\rho}(\mathbf{r},
\mathbf{w}) - 1\\, although the converse is not true and it is possible
to decompose an index into percent-change contributions that do not
allow it to be represented as an arithmetic mean. Note also that other
types of decompositions can be had by setting \\\varsigma\\ to a value
other than 0 or 1; for example, setting \\\varsigma = -1\\ gives the
harmonic decomposition,

\\ \mathfrak{M}\_{\rho}(\mathbf{r}, \mathbf{w}) = 1 /
\left(\sum\_{i=1}^{n} v_i(\mathbf{r}, \mathbf{w}; \rho, -1) /
r\_{i}\right), \\

that will be useful in [Section 2.3](#sec-harmonic).

Balk ([2008](#ref-balk2008)) and Reinsdorf, Diewert, and Ehemann
([2002](#ref-reinsdorf2002)) show how to derive \\\mathbf{v}(\mathbf{r},
\mathbf{w}; \rho, \varsigma)\\ when \\\rho = 1\\ and \\\varsigma = 0\\
(multiplicative decomposition of an arithmetic index) \\
v\_{i}(\mathbf{r}, \mathbf{w}; 1, 0) = \frac{w\_{i} \mathfrak{L}(r\_{i},
\mathfrak{M}\_{1}(\mathbf{r}, \mathbf{w}))}{\sum\_{j=1}^{n} w\_{j}
\mathfrak{L}(r\_{j}, \mathfrak{M}\_{1}(\mathbf{r}, \mathbf{w}))} \\ and
\\\rho = 0\\ and \\\varsigma = 1\\ (additive decomposition of a
geometric index) \\ v\_{i}(\mathbf{r}, \mathbf{w}; 0, 1) = \frac{w\_{i}
/ \mathfrak{L}(r\_{i}, \mathfrak{M}\_{0}(\mathbf{r},
\mathbf{w}))}{\sum\_{j=1}^{n} w\_{j} / \mathfrak{L}(r\_{j},
\mathfrak{M}\_{0}(\mathbf{r}, \mathbf{w}))}, \\ using the logarithmic
mean \\ \mathfrak{L}(a, b) = \begin{cases} \frac{a - b}{\log(a / b)} & a
\neq b\\ a & a = b. \end{cases} \\

Generalizing these results follows from replacing the logarithmic mean
with the more general extended mean ([Bullen 2003,
393](#ref-bullen2003)), defined for any \\a,b \> 0\\ as \\
\mathfrak{E}\_{\rho\varsigma}(a, b) = \begin{cases}
\left(\frac{\varsigma(a^\rho - b^\rho)}{\rho(a^\varsigma -
b^\varsigma)}\right)^{1 / (\rho - \varsigma)} & \rho \neq \varsigma,
\rho \neq 0, \varsigma \neq 0, a \neq b \\ \left(\frac{a^\rho -
b^\rho}{\rho\log(a / b)}\right)^{1 / \rho} & \rho \neq 0, \varsigma = 0,
a \neq b \\ \left(\frac{a^\varsigma - b^\varsigma}{\varsigma\log(a /
b)}\right)^{1 / \varsigma} & \rho = 0, \varsigma \neq 0, a \neq b \\
\frac{1}{\exp(1 / \rho)} \left(\frac{a^{a^\rho}}{b^{b^\rho}}\right)^{1 /
(a^\rho - b^\rho)} & \rho = \varsigma \neq 0, a \neq b \\ \sqrt{ab} &
\rho = \varsigma = 0, a \neq b \\ a & a = b. \end{cases} \\ The extended
mean reduces to the logarithmic mean when either \\\rho = 0\\ and
\\\varsigma = 1\\, or \\\rho = 1\\ and \\\varsigma = 0\\. But using the
extended mean in place of the logarithmic mean allows for decompositions
of indexes based on other types of means, like harmonic indexes (\\\rho
= -1\\) and the Lloyd-Moulton index (\\\rho = 1 - \sigma\\, where
\\\sigma\\ is an elasticity of substitution).

The key to transforming the weights in a generalized mean of order
\\\rho\\ into the weights for a generalized mean of order \\\varsigma\\
comes from noting that the extended mean is always strictly positive and
satisfies the identity \\ \sum\_{i=1}^{n} w\_{i}
\mathfrak{E}\_{\rho\varsigma}(r_i, \mathfrak{M}\_{\rho} (\mathbf{r},
\mathbf{w}))^{\rho - \varsigma} \varepsilon\_{i}(\mathbf{r}, \mathbf{w};
\rho, \varsigma) \equiv 0, \tag{1}\\ where \\
\varepsilon\_{i}(\mathbf{r}, \mathbf{w}; \rho, \varsigma) =
\begin{cases} r_i^\varsigma - \mathfrak{M}\_{\rho}(\mathbf{r},
\mathbf{w})^\varsigma & \text{if } \varsigma \neq 0, \\ \log(r_i) -
\log(\mathfrak{M}\_{\rho}(\mathbf{r}, \mathbf{w})) & \text{if }
\varsigma = 0. \end{cases} \\ [Equation 1](#eq-identity) uses the
extended mean to keep the weighted deviation from the mean constant for
each price relative (up to a common factor of proportionality) when
changing the order of the mean from \\\rho\\ to \\\varsigma\\, without
changing the value of the mean. Rearranging then gives that \\
v\_{i}(\mathbf{r}, \mathbf{w}; \rho, \varsigma) = w\_{i}
\mathfrak{E}\_{\rho\varsigma}(r_i, \mathfrak{M}\_{\rho} (\mathbf{r},
\mathbf{w}))^{\rho - \varsigma} \Bigg/ \sum\_{j=1}^{n} w\_{j}
\mathfrak{E}\_{\rho\varsigma}(r_j, \mathfrak{M}\_{\rho} (\mathbf{r},
\mathbf{w}))^{\rho - \varsigma} \tag{2}\\ is a suitable function to find
weights that turn an index based on a generalized mean of order \\\rho\\
into one based on a generalized mean of order \\\varsigma\\.

The function given by [Equation 2](#eq-res1) takes on all existing
decompositions that I know of as special cases. Setting \\\rho = 0\\ and
\\\varsigma = 1\\, or \\\rho = 1\\ and \\\varsigma = 0\\, gives the
special cases by Balk ([2008](#ref-balk2008)) and Reinsdorf, Diewert,
and Ehemann ([2002](#ref-reinsdorf2002)) for decomposing indexes based
on arithmetic and geometric means (because the extended mean reduces to
the logarithmic mean). Similarly, because
\\\mathfrak{E}\_{\rho\varsigma}(r_i, \mathfrak{M}\_{\rho} (\mathbf{r},
\mathbf{w}))^{\rho - \varsigma} \equiv \left(\mathfrak{M}\_{\rho}
(\mathbf{r}, \mathbf{w}) r\_{i}\right)^\rho\\ when \\\rho = -1\\ and
\\\varsigma = 1\\, or \\\rho = 1\\ and \\\varsigma = -1\\, setting
\\\rho = -1\\ and \\\varsigma = 1\\ reduces [Equation 2](#eq-res1) to \\
v\_{i}(\mathbf{r}, \mathbf{w}; -1, 1) = \frac{w\_{i} /
r\_{i}}{\sum\_{i=1}^{n}w\_{i} / r\_{i}} ; \\ if \\\mathbf{w}\\ is a
vector of current-period expenditure/revenue shares then these are the
hybrid weights that allow a Paasche index to be calculated as an
arithmetic mean of price relatives. Setting \\\rho = 1\\ and \\\varsigma
= -1\\ reduces [Equation 2](#eq-res1) to \\ v\_{i}(\mathbf{r},
\mathbf{w}; 1, -1) = \frac{w\_{i} r\_{i}}{\sum\_{i=1}^{n}w\_{i} r\_{i}}.
\\ If \\\mathbf{w}\\ is a vector of base-period expenditure/revenue
shares then these are the hybrid weights that allow a Laspeyres index to
be calculated as a harmonic mean of price relatives. As should be
expected, the weights are unchanged if \\\rho = \varsigma\\ or each
element of \\\mathbf{r}\\ takes on the same value.

### Transitivity, monotonicity, and uniqueness

The extended mean has two properties that make it useful for decomposing
price indexes. First, the order of the extended mean has a transitivity
property under multiplication: \\\mathfrak{E}\_{\rho\varsigma}(a,
b)^{\rho - \varsigma} = \mathfrak{E}\_{\rho\tau}(a, b)^{\rho -
\tau}\mathfrak{E}\_{\tau\varsigma}(a, b)^{\tau - \varsigma}\\. This
means the function in [Equation 2](#eq-res1) is transitivity, such that
transmuting the weights to turn a generalized mean of order \\\rho\\
into a generalized mean of order \\\tau\\, and then transmuting these
weights again to turn a generalized mean of order \\\tau\\ into a
generalized mean of order \\\varsigma\\, is the same as finding weights
to make a generalized mean of order \\\rho\\ into one of \\\varsigma\\.
That is, \\ \mathbf{v}(\mathbf{r}, \mathbf{v}(\mathbf{r}, \mathbf{w};
\rho, \tau); \tau, \varsigma) \equiv \mathbf{v}(\mathbf{r}, \mathbf{w};
\rho, \varsigma). \\ As will be seen later, not all decompositions have
this property.

Second, the extended mean is a strictly increasing function in both
arguments ([Bullen 2003, 395](#ref-bullen2003)). This means that the
function in [Equation 2](#eq-res1) has a monotonicity property whereby
the transmuted weights increase (decrease) for large (small) price
relatives if and only if \\\rho \> \varsigma\\. That is, assuming
\\\mathbf{r}\\ is ordered from smallest to largest (and does not contain
all the same value), if \\\rho \> \varsigma\\ then there is a pair of
integer \\k,l\\, with \\k\leq l\\, such that \\v\_{i}(\mathbf{r},
\mathbf{w}; \rho, \varsigma) \geq w\_{i}\\ for \\i\geq l\\ and
\\v\_{i}(\mathbf{r}, \mathbf{w}; \rho, \varsigma) \leq w\_{i}\\ for
\\i\leq k\\, with these equalities reversed if \\\rho \<
\varsigma\\.[²](#fn2) Again, not all decompositions satisfy this
property.

The decomposition given by [Equation 2](#eq-res1) is unique when \\n =
2\\ and \\r\_{1} \neq r\_{2}\\, and is the only such function that
always returns \\\mathbf{w}\\ when \\r\_{1} = r\_{2}\\. But
[Equation 2](#eq-res1) is necessarily not unique when \\n\geq3\\, and
there are infinitely many ways to decompose an index based on the
generalized mean.[³](#fn3) Nonetheless, the limited case of uniqueness
will be useful in [Section 2.1](#sec-superlative).

### Numerical example

Consider the following numerical example of decomposing a quadratic-mean
index ([Lippe 2007, 61](#ref-vonderlippe2007)) (i.e., a producer price
index with an elasticity of substitution of -1) made using the price and
quantity data from Balk ([2008](#ref-balk2008), tables 3.1 and 3.2). The
[`transmute_weights()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
function factory parameterizes the decomposition in
[Equation 2](#eq-res1) to make a function that decomposes any
generalized-mean index.

``` r
library(gpindex)

p2 <- price6[[2]]
p1 <- price6[[1]]
q2 <- quantity6[[2]]
q1 <- quantity6[[1]]

rel <- p2 / p1

s1 <- scale_weights(p1 * q1)
s2 <- scale_weights(p2 * q2)

quadratic_mean <- generalized_mean(2)
quadratic_decomposition <- transmute_weights(2, 1)

v <- quadratic_decomposition(rel, s1)

all.equal(
  quadratic_mean(rel, s1),
  arithmetic_mean(rel, v)
)
#> [1] TRUE
```

Because the quadratic mean is larger than the arithmetic mean, the
weight from the smaller price relatives is transferred to the largest
price relative.

``` r
(v - s1)[order(rel)]
#> [1] -0.024395704 -0.010503706 -0.007454243 -0.008131901 -0.003049463
#> [6]  0.053535017
```

It is also readily seen that turning the arithmetic decomposition into a
geometric one is the same as directly constructing the multiplicative
decomposition.

``` r
all.equal(
  transmute_weights(1, 0)(rel, v),
  transmute_weights(2, 0)(rel, s1)
)
#> [1] TRUE
```

## Extensions

### Decomposing superlative indexes

The additive and multiplicative decompositions for the Fisher index by
Reinsdorf, Diewert, and Ehemann ([2002, sec. 6](#ref-reinsdorf2002)) can
be generalized in the same way as the decompositions for the arithmetic
and geometric indexes by noting that the Fisher index is simply a nested
generalized mean of indexes based on the generalized mean. For a pair of
generalized means \\\mathfrak{N}\_{\rho\_{1}\rho\_{2}}(\mathbf{r},
\mathbf{w}\_{1},
\mathbf{w}\_{2})=\left(\mathfrak{M}\_{\rho_1}(\mathbf{r},
\mathbf{w}\_1), \mathfrak{M}\_{\rho_2}(\mathbf{r},
\mathbf{w}\_2)\right)\\ mapping into \\R_2^{++}\\ with weights
\\\mathbf{\omega}=(\omega_1, \omega_2) \in \Delta^1\\, an index based on
nested generalized means is written as \\
\mathfrak{M}\_{\rho}\left(\mathfrak{N}\_{\rho\_{1}\rho\_{2}}(\mathbf{r},
\mathbf{w}\_{1}, \mathbf{w}\_{2}), \mathbf{\omega}\right). \tag{3}\\

The general family of superlative quadratic mean indexes of order
\\\tau\\ comes from setting \\\rho = 0\\, \\\rho_1 = \tau / 2\\, and
\\\rho_2 = -\tau / 2\\ when \\\omega_1 = \omega_2 = 1 / 2\\,
\\\mathbf{w}\_1\\ are base-period expenditure/revenue shares, and
\\\mathbf{w}\_2\\ are current-period expenditure/revenue shares. In
particular, setting \\\tau = 2\\ gives the Fisher index and setting
\\\tau = 1\\ gives the implicit Walsh index. But
[Equation 3](#eq-superlative) covers other types of indexes as well; for
example, setting each element of \\\mathbf{w}\_1\\ and \\\mathbf{w}\_2\\
to \\1 / n\\ when \\\tau = 2\\ gives the Carruthers-Sellwood-Ward-Dalén
index that serves as an estimator for the Fisher index, whereas \\\tau =
1\\ gives the Balk-Walsh index. Setting \\\rho = -1\\ gives the harmonic
analogue of the Fisher index, which is not a superlative quadratic mean
indexes of order \\\tau\\. Finally, setting \\\rho = \rho\_{1} =
\rho\_{2}\\ and \\\mathbf{w}\_{1} = \mathbf{w}\_{2}\\ gives an index
based on a generalized mean of order \\\rho\\, so that the decomposition
of an index based on the generalized mean is a special case of the
decomposition for [Equation 3](#eq-superlative).

An index of the form in [Equation 3](#eq-superlative) can be decomposed
into an index based on the generalized mean of order \\\rho\\ using the
weights in [Equation 2](#eq-res1), as it can be written as the
generalized mean \\ \mathfrak{M}\_{\rho}(\mathbf{r}, \omega_1
\mathbf{v}(\mathbf{r}, \mathbf{w}\_1; \rho_1, \rho) +
\omega_2\mathbf{v}(\mathbf{r}, \mathbf{w}\_2; \rho_2, \rho)). \\ The
transformation in [Equation 2](#eq-res1) then applies as before, just
replacing \\\mathbf{w}\\ with the more complicated weights \\\omega_1
\mathbf{v}(\mathbf{r}, \mathbf{w}\_1; \rho_1, \rho) +
\omega_2\mathbf{v}(\mathbf{r}, \mathbf{w}\_2, \rho_2, \rho)\\, which can
be written as \\ \mathbf{v}(\mathbf{r}, \omega_1 \mathbf{v}(\mathbf{r},
\mathbf{w}\_1; \rho_1, \rho) + \omega_2\mathbf{v}(\mathbf{r},
\mathbf{w}\_2; \rho_2, \rho); \rho, \varsigma). \tag{4}\\ The idea is to
transmute the weights for both inner means to be of the same order as
the outer mean (\\\rho\\) so that they can be added together, and then
transmute these weights to represent the outer generalized mean as a
mean of order \\\varsigma\\. Note that this means the decomposition
satisfies the same transitivity property as [Equation 2](#eq-res1) with
respect to \\\rho\\ and \\\varsigma\\.

Continuing with the previous example, the
[`nested_transmute()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
factory parameterizes the decomposition in [Equation 4](#eq-res2) and
can be used to decompose the Fisher index.

``` r
v1 <- nested_transmute(0, c(1, -1), 1)(rel, s1, s2)

all.equal(fisher_mean(rel, s1, s2), arithmetic_mean(rel, v1))
#> [1] TRUE

all.equal(
  v1,
  quadratic_decomposition(rel, nested_transmute(0, c(1, -1), 2)(rel, s1, s2))
)
#> [1] TRUE
```

An alternative approach to decompose [Equation 3](#eq-superlative) is to
generalize the (additive) van IJzeren decomposition for the Fisher
index, which can be written as \\
v\_{1}(\mathfrak{N}\_{\rho\_{1}\rho\_{2}}(\mathbf{r}, \mathbf{w}\_{1},
\mathbf{w}\_{2}), \mathbf{\omega}; \rho, \varsigma)
\mathbf{v}(\mathbf{r}, \mathbf{w}\_1; \rho_1, \varsigma) +
v\_{2}(\mathfrak{N}\_{\rho\_{1}\rho\_{2}}(\mathbf{r}, \mathbf{w}\_{1},
\mathbf{w}\_{2}), \mathbf{\omega}; \rho, \varsigma)
\mathbf{v}(\mathbf{r}, \mathbf{w}\_2; \rho_2, \varsigma). \tag{5}\\ Note
that for a Fisher index, if \\\varsigma=1\\ then
\\\mathbf{v}(\mathbf{r}, \mathbf{w}\_1; \rho_1, \varsigma)\\ is a vector
of base-period expenditure/revenue shares, \\\mathbf{v}(\mathbf{r},
\mathbf{w}\_2; \rho_2, \varsigma)\\ is a vector of hybrid Paasche
weights, and \\\mathbf{v}(\mathfrak{N}\_{\rho\_{1}\rho\_{2}}(\mathbf{r},
\mathbf{w}\_{1}, \mathbf{w}\_{2}), \mathbf{\omega}; \rho, \varsigma)\\
are the unique weights that decompose the geometric mean of the
Laspeyres and Paasche indexes, which equals the van IJzeren
decomposition. The idea here is to transmute the weights for both the
inner and outer generalized means so that they are means of order
\\\varsigma\\, then take the product of these weights.
[Equation 4](#eq-res2) and [Equation 5](#eq-res3) generally give
different decompositions, as the latter is not transitive, but reduce to
[Equation 2](#eq-res1) when \\\rho = \rho\_{1} = \rho\_{2}\\ and
\\\mathbf{w}\_{1} = \mathbf{w}\_{2}\\. The
[`nested_transmute2()`](https://marberts.github.io/gpindex/reference/transmute_weights.md)
function implements this method.

``` r
v2 <- nested_transmute2(0, c(1, -1), 1)(rel, s1, s2)

all.equal(fisher_mean(rel, s1, s2), arithmetic_mean(rel, v2))
#> [1] TRUE

all.equal(
  v2,
  quadratic_decomposition(rel, nested_transmute2(0, c(1, -1), 2)(rel, s1, s2))
)
#> [1] "Mean relative difference: 7.760085e-05"
```

Overall the difference between the two approaches tends to be small.

``` r
summary(v1 - v2)
#>       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#> -7.726e-06 -3.059e-06  1.977e-07  0.000e+00  2.617e-06  8.052e-06
```

### Decomposing aggregated indexes

The decomposition in [Equation 5](#eq-res3) can be extended to get a
decomposition for an index constructed using the usual two-step
procedure to make a price index. If price relatives are partitioned into
\\k\\ groups, and an index is made from \\k\\ sub-indexes
\\\mathfrak{N}\_{\rho\_{1}\ldots\rho\_{k}}(\mathbf{r}, \mathbf{w}\_{1},
\ldots, \mathbf{w}\_{k}) = \left(\mathfrak{M}\_{\rho_1}(\mathbf{r}\_{1},
\mathbf{w}\_{1}), \ldots, \mathfrak{M}\_{\rho_k}(\mathbf{r}\_{k},
\mathbf{w}\_{k})\right)\\ with weights
\\\mathbf{\omega}=(\omega\_{1},\ldots,\omega\_{k})\\ as a generalized
mean of order \\\rho\\, then

\\ v\_{i}(\mathfrak{N}\_{\rho\_{1}\ldots\rho\_{k}}(\mathbf{r},
\mathbf{w}\_{1}, \ldots, \mathbf{w}\_{k}), \mathbf{\omega}; \rho,
\varsigma) \mathbf{v}(\mathbf{r}\_{i}, \mathbf{w}\_i; \rho\_{i},
\varsigma) \\

decomposes how the components of the \\i\\-th sub-index contribute
towards the total index. Concatenating each of these vectors of weights
along with the vector of price relatives represents the nested mean over
the partitioning of price relatives as a generalized mean of order
\\\varsigma\\.

``` r
group <- rep(c("a", "b"), each = 3)

s1_by_group <- split(s1, group)
rel_by_group <- split(rel, group)

index_a <- quadratic_mean(rel_by_group$a, s1_by_group$a)
index_b <- geometric_mean(rel_by_group$b)

quadratic_mean(c(index_a, index_b), sapply(s1_by_group, sum))
#> [1] 1.375553

decomp_a <- quadratic_decomposition(rel_by_group$a, s1_by_group$a)
decomp_b <- transmute_weights(0, 1)(rel_by_group$b)

v <- Map(
  `*`,
  quadratic_decomposition(c(index_a, index_b), sapply(s1_by_group, sum)),
  list(decomp_a, decomp_b)
) |>
  unlist()

arithmetic_mean(rel, v)
#> [1] 1.375553
```

### Decomposing deflators

The decomposition in [Equation 2](#eq-res1) can be used to construct a
decomposition for how a price index deflates a change in aggregate value
over time to get a quantity index. Letting \\V\\ be the change in total
value between two periods, the implicit quantity index for a price index
based on the generalized mean of order \\\rho\\ is

\\ \frac{V}{\mathfrak{M}\_{\rho}(\mathbf{r}, \mathbf{w})}. \\

Using [Equation 2](#eq-res1) to find weights to represent
\\\mathfrak{M}\_{\rho}(\mathbf{r}, \mathbf{w})\\ as a harmonic mean
means that

\\ V/ \mathfrak{M}\_{\rho}(\mathbf{r}, \mathbf{w}) \equiv
\mathfrak{M}\_{\varsigma}(V / \mathbf{r}, \mathbf{v}(\mathbf{r},
\mathbf{w}; \rho, -\varsigma)), \tag{6}\\ where \\V / \mathbf{r} =
\left(V / r\_{1}, \ldots, V / r\_{n}\right)\\.

Setting \\\varsigma=1\\ then gives an additive decomposition for how
each price relative acts to deflate the change in total value over time,
while setting \\\varsigma=0\\ gives a multiplicative decomposition. The
same holds true when deflating with an index based on nested means,
using either [Equation 4](#eq-res2) or [Equation 5](#eq-res3).

``` r
V <- sum(p2 * q2) / sum(p1 * q1)

v <- nested_transmute2(0, c(1, -1), -1)(rel, s1, s2)

all.equal(
  arithmetic_mean(V / rel, v),
  fisher_mean(q2 / q1, s1, s2)
)
#> [1] TRUE

V / rel * v
#> [1] 0.08971270 0.09467911 0.19432463 0.11454476 0.45833768 0.05478917
```

### Decomposing contra-harmonic indexes

von der Lippe ([2015](#ref-vonderlippe2015)) discusses using the
contra-harmonic mean (or antiharmonic as he calls it), a special case of
the Lehmer mean ([Bullen 2003, 245](#ref-bullen2003)) (of order 2), to
make price indexes.

``` r
# Arithmetic hybrid index
all.equal(
  arithmetic_mean(p2 / p1, p2 * q1),
  contraharmonic_mean(p2 / p1, p1 * q1)
)
#> [1] TRUE

# Palgrave index
all.equal(
  arithmetic_mean(p2 / p1, p2 * q2),
  contraharmonic_mean(p2 / p1, p1 * q2)
)
#> [1] TRUE
```

The Lehmer mean of order \\\rho\\ is simply the arithmetic mean of the
with weights

\\ \left(w\_{1}r\_{1}^{\rho - 1}, \ldots, w\_{n}r\_{n}^{\rho - 1}\right)
/ \sum\_{i = 1}^{n} w\_{i} r\_{i}^{\rho - 1}, \\

and so indexes based on these means can be decomposed with
[Equation 2](#eq-res1) by using the these weights instead of
\\\mathbf{w}\\.

## References

Balk, B. M. 2008. *Price and Quantity Index Numbers*. Cambridge
University Press.

Bullen, P. S. 2003. *Handbook of Means and Their Inequalities*. Springer
Science+Business Media.

Diewert, W. E. 2002. “The Quadratic Approximation Lemma and
Decompositions of Superlative Indexes.” *Journal of Economic and Social
Measurement* 28 (1-2): 63–88.

Hallerbach, W. G. 2005. “An Alternative Decomposition of the Fisher
Index.” *Economics Letters* 86 (2): 147–52.

Lent, J., and A. H. Dorfman. 2009. “Using a Weighted Average of Base
Period Price Indexes to Approximate a Superlative Index.” *Journal of
Official Statistics* 25 (1): 149–49.

Lippe, Peter von der. 2007. *Index Theory and Price Statistics*. Peter
Lang.

Martin, S. 2021. “A Note on Generalized Decompositions for Price
Indexes.” Statistics Canada.

Reinsdorf, M. B., W. E. Diewert, and C. Ehemann. 2002. “Additive
Decompositions for Fisher, Törnqvist and Geometric Mean Indexes.”
*Journal of Economic and Social Measurement* 28 (1-2): 51–61.

von der Lippe, P. 2015. “Generalized Statistical Means and New Price
Index Formulas, Notes on Some Unexplored Index Formulas, Their
Interpretations and Generalizations.” Munich Personal RePEc Archive
paper no. 64952.

Webster, M. B., and R. C. Tarnow-Mordi. 2019. “Decomposing Multilateral
Price Indexes into the Contributions of Individual Commodities.”
*Journal of Official Statistics* 35: 461–86.

------------------------------------------------------------------------

1.  There are decompositions with “weights” that do not sum to one, such
    as those for the Fisher index by Reinsdorf, Diewert, and Ehemann
    ([2002, sec. 2](#ref-reinsdorf2002)) and Hallerbach
    ([2005](#ref-hallerbach2005)), and for other superlative indexes by
    Diewert ([2002](#ref-diewert2002)). Balk ([2008](#ref-balk2008),
    equation 4.28) gives the original multiplicative decomposition of
    the Fisher index, due to Vartia, for which the weights also do not
    sum to one. Webster and Tarnow-Mordi ([2019](#ref-webster2019))
    consider several decompositions for multilateral indexes, although
    again the “weights” do not sum to one.

2.  These are weak inequalities to account for the case that some
    weights are 0. If the vector if weights is strictly positive then
    the inequalities are strict.

3.  Take, for example, (1/2, 1, 3/2) with weights (1/6, 1/3, 1/2). The
    weighted harmonic mean is 1, which can be computed as an arithmetic
    mean with weights (1/3, 1/3, 1/3) or (1/4, 1/2, 1/4). Hence there
    are multiple additive decompositions for the harmonic mean.
