

#' Generalized Price and Quantity Indexes
#' 
#' A small package for calculating lots of different price indexes, and by
#' extension quantity indexes. Provides tools to build and work with any type
#' of bilateral generalized-mean index (of which most price indexes are), along
#' with a few important indexes that don't belong to the generalized-mean
#' family (e.g., superlative quadratic-mean indexes, GEKS). Implements and
#' extends many of the methods in Balk (2008), von der Lippe (2001), and the
#' CPI manual (2020) for bilateral price indexes.
#' 
#' 
#' @name gpindex-package
#' @aliases gpindex-package gpindex
#' @docType package
#' @section Background: Everything is framed as a price index to avoid
#' duplication; it is trivial to turn a price index into its analogous quantity
#' index by simply switching prices and quantities.
#' 
#' Generalized-mean price indexes (sometimes called generalized price indexes
#' for short) are a large family of price indexes with nice properties, such as
#' the mean-value and identity properties (e.g., Balk, 2008, Chapter 3). When
#' used with value-share weights, these indexes satisfy the key homogeneity
#' properties, commensurability, and are consistent in aggregation. This last
#' feature makes generalized-mean indexes natural candidates for making
#' national statistics, and this justifies the hierarchical structure used by
#' national statistical agencies for calculating and disseminating collections
#' of price indexes.
#' 
#' Almost all bilateral price indexes used in practice are either
#' generalized-mean indexes (like the Laspeyres and Paasche index) or are
#' nested generalized-mean indexes (like the Fisher index).
#' @author **Maintainer**: Steve Martin \email{stevemartin041@@gmail.com}
#' @seealso <https://github.com/marberts/gpindex>
#' @references Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#' 
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). *Consumer Price
#' Index Manual: Theory and Practice*. International Monetary Fund.
#' 
#' von der Lippe, P. (2001). *Chain Indices: A Study in Price Index
#' Theory*, Spectrum of Federal Statistics vol. 16. Federal Statistical Office,
#' Wiesbaden.
#' 
#' Selvanathan, E. A. and Rao, D. S. P. (1994). *Index Numbers: A
#' Stochastic Approach*. MacMillan.
NULL






NULL






#' 
NULL





#' Function operators
#' 
#' Operators to augment the way a function behaves. \itemize{
#' \item`quantity_index()` remaps price arguments into quantity argument
#' (and vice versa) to turn a price index into a quantity index.
#' \item`grouped()` makes a function applicable to grouped data.
#' \item`balanced()` makes a function balance the removal of `NA`s
#' across multiple input vectors. }
#' 
#' 
#' @aliases quantity_index grouped balanced
#' @param f A function.
#' @param ... Additional arguments to `f` that should *not* be
#' treated as grouped / should *not* be balanced.
#' @return `quantity_index()` returns a function like `f`, except
#' that the role of prices/quantities is reversed.
#' 
#' `grouped()` returns a function like `f` with a new argument
#' `group`. This accepts a factor to split all other arguments in `f`
#' (except those specified in `...`) into groups before applying `f`
#' to each group and combining the results. It is similar to
#' [`ave()`][ave], but more general.
#' 
#' `balanced()` returns a function like `f`, except that missing
#' values are balanced across *all* inputs when `na.rm = TRUE`
#' (except those specified in `...`). This is like using
#' [`complete.cases()`][complete.cases] on the inputs of `f`.
#' @note Additional arguments passed to `...` are evaluated, so the return
#' function doesn't evaluate these arguments lazily. In most cases these
#' additional arguments are parameters like `elasticity` or switches like
#' `na.rm`, and eager evaluation prevents these from being changed in,
#' e.g., a loop. Lazy evaluation can be had by passing an anonymous function
#' that partials out these arguments.
#' @seealso [price_index()] for the possible functions that can serve
#' as inputs for `quantity_index()` and `balanced()`.
#' @examples
#' 
#' p1 <- price6[[3]]
#' p0 <- price6[[2]]
#' q1 <- quantity6[[3]]
#' q0 <- quantity6[[2]]
#' 
#' #---- Quantity index ----
#' # Remap argument names to be quantities rather than prices
#' 
#' quantity_index(laspeyres_index)(q1 = q1, q0 = q0, p0 = p0)
#' 
#' laspeyres_index(p1 = q1, p0 = q0, q0 = p0)
#' 
#' # Works with the index_weights() functions, too
#' 
#' quantity_index(index_weights("Laspeyres"))(q0 = q0, p0 = p0)
#' 
#' #---- Group ----
#' # Calculate Tornqvist weights for two groups
#' 
#' f <- factor(rep(letters[1:2], each = 3))
#' tornqvist_weights <- grouped(index_weights("Tornqvist"))
#' tornqvist_weights(p1, p0, q1, q0, group = f)
#' 
#' # Calculate a mean like ave(), but with weights
#' 
#' x <- 1:6
#' w <- c(1:5, NA)
#' grouped_mean <- grouped(geometric_mean, na.rm = TRUE)
#' grouped_mean(x, w, group = f)
#' 
#' # Redistribute weights
#' 
#' w1 <- c(2, 4)
#' w2 <- 1:6
#' 
#' harmonic_mean(mapply(harmonic_mean, split(x, f), split(w2, f)), w1)
#' 
#' wr <- grouped(scale_weights)(w2, group = f) * w1[f]
#' harmonic_mean(x, wr)
#' 
#' #---- Balance ----
#' # Balance missing values for a Fisher index
#' 
#' fisher <- balanced(fisher_index)
#' fisher(p1, p0, q1, replace(q0, 3, NA), na.rm = TRUE)
#' fisher_index(p1[-3], p0[-3], q1[-3], q0[-3])
#' 
#' # Operators can be combined, but some care may be needed
#' 
#' grouped(balanced(fisher_mean), na.rm = TRUE)(x, w, group = f)
#' balanced(grouped(fisher_mean))(x, w, group = f, na.rm = TRUE)
#' 
NULL






NULL





#' Sample price/quantity data
#' 
#' Prices and quantities for six products over five periods.
#' 
#' 
#' @name price_data
#' @aliases price6 quantity6
#' @docType data
#' @format Each data frame has 6 rows and 5 columns, with each row
#' corresponding to a product and each column corresponding to a time period.
#' @note Adapted from tables 3.1 and 3.2 in Balk (2008), which were adapted
#' from tables 19.1 and 19.2 in the PPI manual.
#' @source Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#' 
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). *Producer Price
#' Index Manual: Theory and Practice*. International Monetary Fund.
#' @examples
#' 
#' # Recreate tables 3.4, 3.6, and 3.12 from Balk (2008)
#' 
#' index_formulas <- function(p1, p0, q1, q0) {
#'   c(harmonic_laspeyres = harmonic_index("Laspeyres")(p1, p0, q0),
#'     geometric_laspeyres = geometric_index("Laspeyres")(p1, p0, q0),
#'     laspeyres = arithmetic_index("Laspeyres")(p1, p0, q0),
#'     paasche = harmonic_index("Paasche")(p1, p0, q1),
#'     geometric_paasche = geometric_index("Paasche")(p1, p0, q1),
#'     palgrave = arithmetic_index("Palgrave")(p1, p0, q1),
#'     fisher = fisher_index(p1, p0, q1, q0),
#'     tornqvist = geometric_index("Tornqvist")(p1, p0, q1, q0),
#'     marshall_edgeworth = arithmetic_index("MarshallEdgeworth")(p1, p0, q1, q0),
#'     walsh1 = arithmetic_index("Walsh1")(p1, p0, q1, q0),
#'     vartia2 = geometric_index("Vartia2")(p1, p0, q1, q0),
#'     vartia1 = geometric_index("Vartia1")(p1, p0, q1, q0),
#'     stuvel = stuvel_index(2, 2)(p1, p0, q1, q0)
#'   )
#' }
#' 
#' round(t(mapply(index_formulas, price6, price6[1], quantity6, quantity6[1])), 4)
#' 
NULL





#' Price indexes
#' 
#' Calculate a variety of price indexes using information on prices and
#' quantities at two points in time.
#' 
#' The `arithmetic_index()`, `geometric_index()`, and
#' `harmonic_index()` functions return a function to calculate a given
#' type of arithmetic, geometric (logarithmic), and harmonic index. Together,
#' these functions produce functions to calculate the following indexes.
#' \itemize{ \item **Arithmetic indexes** \item Carli \item Dutot \item
#' Laspeyres \item Palgrave \item Unnamed index (arithmetic mean of Laspeyres
#' and Palgrave) \item Drobisch (arithmetic mean of Laspeyres and Paasche)
#' \item Walsh-I (arithmetic Walsh) \item Marshall-Edgeworth \item Geary-Khamis
#' \item Lowe \item Young \item **Geometric indexes** \item Jevons \item
#' Geometric Laspeyres \item Geometric Paasche \item Geometric Young \item
#' Törnqvist (or Törnqvist-Theil) \item Montgomery-Vartia / Vartia-I \item
#' Sato-Vartia / Vartia-II \item Walsh-II (geometric Walsh) \item Theil \item
#' Rao \item **Harmonic indexes** \item Coggeshall (equally weighted
#' harmonic index) \item Paasche \item Harmonic Laspeyres \item Harmonic Young
#' }
#' 
#' Along with the `lm_index()` function to calculate the Lloyd-Moulton
#' index, these are just convenient wrappers for
#' [`generalized_mean()`][generalized_mean] and
#' `index_weights()`.
#' 
#' The Laspeyres, Paasche, Jevons, Lowe, and Young indexes are among the most
#' common price indexes, and so they get their own functions. The
#' `laspeyres_index()`, `lowe_index()`, and `young_index()`
#' functions correspond to setting the appropriate `type` in
#' `arithmetic_index()`; `paasche_index()` and `jevons_index()`
#' instead come from the `harmonic_index()` and `geometric_index()`
#' functions.
#' 
#' In addition to these indexes, there are also functions for calculating a
#' variety of indexes not based on generalized means. The Fisher index is the
#' geometric mean of the arithmetic Laspeyres and Paasche indexes; the Harmonic
#' Laspeyres Paasche index is the harmonic analog of the Fisher index (8054 on
#' Fisher's list). The Carruthers-Sellwood-Ward-Dalen and
#' Carruthers-Sellwood-Ward-Dalen-Balk indexes are sample analogs of the Fisher
#' index; the Balk-Walsh index is the sample analog of the Walsh index. The AG
#' mean index is the arithmetic or geometric mean of the geometric and
#' arithmetic Laspeyres indexes, weighted by the elasticity of substitution.
#' The `stuvel_index()` function returns a function to calculate a Stuvel
#' index of the given parameters. The Lehr index is an alternative to the
#' Geary-Khamis index, and is the implicit price index for Fisher's index 4153.
#' 
#' The `index_weights()` function returns a function to calculate weights
#' for a variety of price indexes. Weights for the following types of indexes
#' can be calculated. \itemize{ \item Carli / Jevons / Coggeshall \item Dutot
#' \item Laspeyres / Lloyd-Moulton \item Hybrid Laspeyres (for use in a
#' harmonic mean) \item Paasche / Palgrave \item Hybrid Paasche (for use in an
#' arithmetic mean) \item Törnqvist / Unnamed \item Drobisch \item Walsh-I (for
#' an arithmetic Walsh index) \item Walsh-II (for a geometric Walsh index)
#' \item Marshall-Edgeworth \item Geary-Khamis \item Montgomery-Vartia /
#' Vartia-I \item Sato-Vartia / Vartia-II \item Theil \item Rao \item Lowe
#' \item Young }
#' 
#' The weights need not sum to 1, as this normalization isn't always
#' appropriate (i.e., for the Vartia-I weights).
#' 
#' Naming for the indexes and weights generally follows the CPI manual (2020),
#' Balk (2008), and Selvanathan and Rao (1994). In several cases two or more
#' names correspond to the same weights (e.g., Paasche and Palgrave, or
#' Sato-Vartia and Vartia-II). The calculations are given in the examples.
#' 
#' @aliases price_index arithmetic_index geometric_index harmonic_index
#' laspeyres_index paasche_index jevons_index lowe_index young_index
#' fisher_index hlp_index lm_index arithmetic_agmean_index
#' geometric_agmean_index cswd_index cswdb_index bw_index stuvel_index
#' index_weights lehr_index
#' @param type The name of the index. See details for the possible types of
#' indexes.
#' @param elasticity The elasticity of substitution for the Lloyd-Moulton and
#' AG mean indexes.
#' @param a,b Parameters for the generalized Stuvel index.
#' @param p1 Current-period prices.
#' @param p0 Base-period prices.
#' @param q1 Current-period quantities.
#' @param q0 Base-period quantities.
#' @param pb Period-b prices for the Lowe/Young index.
#' @param qb Period-b quantities for the Lowe/Young index.
#' @param na.rm Should missing values be removed? By default missing values for
#' prices or quantities return a missing value.
#' @return `arithmetic_index()`, `geometric_index()`,
#' `harmonic_index()`, `stuvel_index()`, and `index_weights()`
#' each return a function to compute the relevant price indexes;
#' `lm_index()`, `arithmetic_agmean_index()`, and
#' `geometric_agmean_index()` each return a function to calculate the
#' relevant index for a given elasticity of substitution. The others return a
#' numeric value giving the change in price between the base period and current
#' period.
#' @note There are different ways to deal with missing values in a price index,
#' and care should be taken when relying on these functions to remove missing
#' values. Setting `na.rm = TRUE` removes price relatives with missing
#' information, either because of a missing price or a missing weight, while
#' using all available non-missing information to make the weights.
#' 
#' Certain properties of an index-number formula may not work as expected when
#' removing missing values if there is ambiguity about how to remove missing
#' values from the weights (as in, e.g., a Törnqvist or Sato-Vartia index). The
#' [`balanced()`][balanced] operator may be helpful, as it balances
#' the removal of missing values across prices and quantities prior to making
#' the weights.
#' @seealso [generalized_mean()] for the generalized mean that powers
#' most of these functions.
#' 
#' [contributions()] for calculating percent-change contributions.
#' 
#' [update_weights()] for price-updating weights.
#' 
#' [quantity_index()] to remap the arguments in these functions for a
#' quantity index.
#' 
#' [geks()] for making a GEKS index based on, e.g., the Fisher index.
#' 
#' [price6()] for an example of how to use these functions with more
#' than two time periods.
#' 
#' The \pkg{piar} package has more functionality working with price indexes for
#' multiple groups of products over many time periods.
#' @references Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#' 
#' Fisher, I. (1922). *The Making of Index Numbers*. Houghton Mifflin
#' Company.
#' 
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). *Consumer Price
#' Index Manual: Theory and Practice*. International Monetary Fund.
#' 
#' von der Lippe, P. (2001). *Chain Indices: A Study in Price Index
#' Theory*, Spectrum of Federal Statistics vol. 16. Federal Statistical Office,
#' Wiesbaden.
#' 
#' von der Lippe, P. (2015). Generalized Statistical Means and New Price Index
#' Formulas, Notes on some unexplored index formulas, their interpretations and
#' generalizations. Munich Personal RePEc Archive paper no. 64952.
#' 
#' Selvanathan, E. A. and Rao, D. S. P. (1994). *Index Numbers: A
#' Stochastic Approach*. MacMillan.
#' @examples
#' 
#' p0 <- price6[[2]]
#' p1 <- price6[[3]]
#' q0 <- quantity6[[2]]
#' q1 <- quantity6[[3]]
#' pb <- price6[[1]]
#' qb <- quantity6[[1]]
#' 
#' #---- Calculating price indexes ----
#' 
#' # Most indexes can be calculated by combining the appropriate weights 
#' # with the correct type of mean
#' 
#' geometric_index("Laspeyres")(p1, p0, q0)
#' geometric_mean(p1 / p0, index_weights("Laspeyres")(p0, q0))
#' 
#' # Arithmetic Laspeyres index
#' 
#' laspeyres_index(p1, p0, q0)
#' arithmetic_mean(p1 / p0, index_weights("Laspeyres")(p0, q0)) 
#' 
#' # Harmonic calculation for the arithmetic Laspeyres
#' 
#' harmonic_mean(p1 / p0, index_weights("HybridLaspeyres")(p1, q0))
#' 
#' # Same as transmuting the weights
#' 
#' all.equal(
#'   scale_weights(index_weights("HybridLaspeyres")(p1, q0)),
#'   scale_weights(
#'     transmute_weights(1, -1)(p1 / p0, index_weights("Laspeyres")(p0, q0))
#'   )
#' )
#' 
#' # This strategy can be used to make more exotic indexes, like the 
#' # quadratic-mean index (von der Lippe, 2001, p. 71)
#' 
#' generalized_mean(2)(p1 / p0, index_weights("Laspeyres")(p0, q0))
#' 
#' # Or the exponential mean index (p. 64)
#' 
#' log(arithmetic_mean(exp(p1 / p0), index_weights("Laspeyres")(p0, q0)))
#' 
#' # Or the arithmetic hybrid index (von der Lippe, 2015, p. 5)
#' 
#' arithmetic_mean(p1 / p0, index_weights("HybridLaspeyres")(p1, q0))
#' contraharmonic_mean(p1 / p0, index_weights("Laspeyres")(p0, q0))
#' 
#' # Unlike its arithmetic counterpart, the geometric Laspeyres can 
#' # increase when base-period prices increase if some of these prices 
#' # are small
#' 
#' gl <- geometric_index("Laspeyres")
#' p0_small <- replace(p0, 1, p0[1] / 5)
#' p0_dx <- replace(p0_small, 1, p0_small[1] + 0.01)
#' gl(p1, p0_small, q0) < gl(p1, p0_dx, q0)
#' 
#' #---- Price updating the weights in a price index ----
#' 
#' # Chain an index by price updating the weights
#' 
#' p2 <- price6[[4]]
#' laspeyres_index(p2, p0, q0)
#' 
#' I1 <- laspeyres_index(p1, p0, q0) 
#' w_pu <- update_weights(p1 / p0, index_weights("Laspeyres")(p0, q0))
#' I2 <- arithmetic_mean(p2 / p1, w_pu)
#' I1 * I2
#' 
#' # Works for other types of indexes, too
#' 
#' harmonic_index("Laspeyres")(p2, p0, q0)
#' 
#' I1 <- harmonic_index("Laspeyres")(p1, p0, q0) 
#' w_pu <- factor_weights(-1)(p1 / p0, index_weights("Laspeyres")(p0, q0))
#' I2 <- harmonic_mean(p2 / p1, w_pu)
#' I1 * I2
#' 
#' #---- Percent-change contributions ----
#' 
#' # Percent-change contributions for the Tornqvist index
#' 
#' w <- index_weights("Tornqvist")(p1, p0, q1, q0)
#' (con <- geometric_contributions(p1 / p0, w))
#' 
#' all.equal(sum(con), geometric_index("Tornqvist")(p1, p0, q1, q0) - 1)
#' 
#' #---- Missing values ----
#' 
#' # NAs get special treatment
#' 
#' p_na <- replace(p0, 6, NA)
#' 
#' # Drops the last price relative
#' 
#' laspeyres_index(p1, p_na, q0, na.rm = TRUE)
#' 
#' # Only drops the last period-0 price
#' 
#' sum(p1 * q0, na.rm = TRUE) / sum(p_na * q0, na.rm = TRUE)
#' 
#' #---- von Bortkiewicz decomposition ----
#' 
#' paasche_index(p1, p0, q1) / laspeyres_index(p1, p0, q0) - 1
#' 
#' wl <- scale_weights(index_weights("Laspeyres")(p0, q0))
#' pl <- laspeyres_index(p1, p0, q0)
#' ql <- quantity_index(laspeyres_index)(q1, q0, p0)
#' 
#' sum(wl * (p1 / p0 / pl - 1) * (q1 / q0 / ql - 1))
#' 
#' # Similar decomposition for geometric Laspeyres/Paasche
#' 
#' wp <- scale_weights(index_weights("Paasche")(p1, q1))
#' gl <- geometric_index("Laspeyres")(p1, p0, q0)
#' gp <- geometric_index("Paasche")(p1, p0, q1)
#' 
#' log(gp / gl)
#' 
#' sum(scale_weights(wl) * (wp / wl - 1) * log(p1 / p0 / gl))
#' 
#' #---- Consistency in aggregation ----
#' 
#' p0a <- p0[1:3]; p0b <- p0[4:6]
#' p1a <- p1[1:3]; p1b <- p1[4:6]
#' q0a <- q0[1:3]; q0b <- q0[4:6]
#' q1a <- q1[1:3]; q1b <- q1[4:6]
#' 
#' # Indexes based on the generalized mean with value share weights are 
#' # consistent in aggregation
#' 
#' lm_index(0.75)(p1, p0, q0)
#' 
#' w <- index_weights("LloydMoulton")(p0, q0)
#' Ia <- generalized_mean(0.25)(p1a / p0a, w[1:3])
#' Ib <- generalized_mean(0.25)(p1b / p0b, w[4:6])
#' generalized_mean(0.25)(c(Ia, Ib), c(sum(w[1:3]), sum(w[4:6])))
#' 
#' # Agrees with group-wise indexes
#' 
#' all.equal(lm_index(0.75)(p1a, p0a, q0a), Ia)
#' all.equal(lm_index(0.75)(p1b, p0b, q0b), Ib)
#' 
#' # Care is needed with more complex weights, e.g., Drobisch, as this 
#' # doesn't fit Balk's (2008) definition (p. 113) of a generalized-mean 
#' # index (it's the arithmetic mean of a Laspeyres and Paasche index)
#' 
#' arithmetic_index("Drobisch")(p1, p0, q1, q0)
#' 
#' w <- index_weights("Drobisch")(p1, p0, q1, q0)
#' Ia <- arithmetic_mean(p1a / p0a, w[1:3])
#' Ib <- arithmetic_mean(p1b / p0b, w[4:6])
#' arithmetic_mean(c(Ia, Ib), c(sum(w[1:3]), sum(w[4:6])))
#' 
#' # Does not agree with group-wise indexes
#' 
#' all.equal(arithmetic_index("Drobisch")(p1a, p0a, q1a, q0a), Ia)
#' all.equal(arithmetic_index("Drobisch")(p1b, p0b, q1b, q0b), Ib)
#' 
#' #---- Making the weights for different indexes ----
#' 
#' # Explicit calculation for each of the different weights
#' # Carli/Jevons/Coggeshall
#' 
#' all.equal(index_weights("Carli")(p1), rep(1, length(p0)))
#' 
#' # Dutot
#' 
#' all.equal(index_weights("Dutot")(p0), p0)
#' 
#' # Laspeyres / Lloyd-Moulton
#' 
#' all.equal(index_weights("Laspeyres")(p0, q0), p0 * q0)
#' 
#' # Hybrid Laspeyres
#' 
#' all.equal(index_weights("HybridLaspeyres")(p1, q0), p1 * q0)
#' 
#' # Paasche / Palgrave
#' 
#' all.equal(index_weights("Paasche")(p1, q1), p1 * q1)
#' 
#' # Hybrid Paasche
#' 
#' all.equal(index_weights("HybridPaasche")(p0, q1), p0 * q1)
#' 
#' # Tornqvist / Unnamed
#' 
#' all.equal(index_weights("Tornqvist")(p1, p0, q1, q0),
#'           0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1))
#' 
#' # Drobisch
#' 
#' all.equal(index_weights("Drobisch")(p1, p0, q1, q0),
#'           0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p0 * q1 / sum(p0 * q1))
#' 
#' # Walsh-I
#' 
#' all.equal(index_weights("Walsh1")(p0, q1, q0),
#'           p0 * sqrt(q0 * q1))
#' 
#' # Marshall-Edgeworth
#' 
#' all.equal(index_weights("MarshallEdgeworth")(p0, q1, q0),
#'           p0 * (q0 + q1))
#' 
#' # Geary-Khamis
#' 
#' all.equal(index_weights("GearyKhamis")(p0, q1, q0),
#'           p0 / (1 / q0 + 1 / q1))
#' 
#' # Montgomery-Vartia / Vartia-I
#' 
#' all.equal(
#'   index_weights("MontgomeryVartia")(p1, p0, q1, q0),
#'   logmean(p0 * q0, p1 * q1) / logmean(sum(p0 * q0), sum(p1 * q1))
#' )
#' 
#' # Sato-Vartia / Vartia-II
#' 
#' all.equal(index_weights("SatoVartia")(p1, p0, q1, q0),
#'           logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)))
#' 
#' # Walsh-II
#' 
#' all.equal(index_weights("Walsh2")(p1, p0, q1, q0),
#'           sqrt(p0 * q0 * p1 * q1))
#'           
#' # Theil
#' 
#' all.equal(index_weights("Theil")(p1, p0, q1, q0),
#'           {w0 <- scale_weights(p0 * q0);
#'            w1 <- scale_weights(p1 * q1);
#'            (w0 * w1 * (w0 + w1) / 2)^(1 / 3)})
#'            
#' # Rao
#' 
#' all.equal(index_weights("Rao")(p1, p0, q1, q0),
#'           {w0 <- scale_weights(p0 * q0);
#'            w1 <- scale_weights(p1 * q1);
#'            w0 * w1 / (w0 + w1)})
#' 
#' # Lowe
#' 
#' all.equal(index_weights("Lowe")(p0, qb), p0 * qb)
#' 
#' # Young
#' 
#' all.equal(index_weights("Young")(pb, qb), pb * qb)
#' 
NULL





#' Transform weights
#' 
#' Useful transformations for the weights in a generalized mean.  \itemize{
#' \itemTransmute weights to turn a generalized mean of order \eqn{r} into a
#' generalized mean of order \eqn{s}. Useful for calculating additive and
#' multiplicative decompositions for a generalized-mean index, and those made
#' of nested generalized means (e.g., Fisher index). \itemFactor weights to
#' turn the generalized mean of a product into the product of generalized
#' means. Useful for price-updating the weights in a generalized-mean index.
#' \itemScale weights so they sum to 1. }
#' 
#' The function `transmute_weights()` returns a function to compute a
#' vector of weights `v(x, w)` such that
#' 
#' \preformatted{generalized_mean(r)(x, w) == generalized_mean(s)(x, v(x, w))}
#' 
#' `nested_transmute()` and `nested_transmute2()` do the same for
#' nested generalized means, so that
#' 
#' \preformatted{nested_mean(r1, r2, t)(x, w1, w2) == generalized_mean(s)(x,
#' v(x, w1, w2))}
#' 
#' This generalizes the result for turning a geometric mean into an arithmetic
#' mean (and vice versa) in section 4.2 of Balk (2008), and a Fisher mean into
#' an arithmetic mean in section 6 of Reinsdorf et al. (2002), although these
#' are usually the most important cases. See Martin (2021) for details.
#' `nested_transmute2()` takes a slightly different approach than
#' `nested_transmute()`, generalizing the van IJzeren arithmetic
#' decomposition for the Fisher index (Balk, 2008, section 4.2.2) using the
#' approach by Martin (2021), although in most cases the results are broadly
#' similar.
#' 
#' The function `factor_weights()` returns a function to compute weights
#' `u(x, w)` such that
#' 
#' \preformatted{ generalized_mean(r)(x * y, w) == generalized_mean(r)(x, w) *
#' generalized_mean(r)(y, u(x, w)) }
#' 
#' This generalizes the result in section C.5 of Chapter 9 of the PPI Manual
#' for chaining the Young index, and gives a way to chain generalized-mean
#' price indexes over time. Factoring weights with `r = 1` sometimes gets
#' called price-updating weights; `update_weights()` simply calls
#' `factor_weights(1)()`.
#' 
#' The function `scale_weights()` scales a vector of weights so they sum
#' to 1 by calling `x / sum(x, na.rm = TRUE)`.
#' 
#' Both `x` and `w` should be strictly positive. This is not
#' enforced, but the results may not make sense in cases where the generalized
#' mean and generalized logarithmic mean are not defined.
#' 
#' @aliases transmute_weights nested_transmute nested_transmute2 factor_weights
#' update_weights scale_weights
#' @param r,s A finite number giving the order of the generalized mean. See
#' details.
#' @param r1,r2,t The same arguments as in
#' [`nested_mean()`][nested_mean]. See details.
#' @param x A strictly positive numeric vector.
#' @param w A strictly positive numeric vector of weights, the same length as
#' `x`. The default is to equally weight each element of `x`.
#' @return `transmute_weights()` and `factor_weights()` return a
#' function:
#' 
#' \preformatted{function(x, w = NULL){...}}
#' 
#' In each case this function augments the weights `w`.
#' 
#' `nested_transmute()` and `nested_transmute2()` similarly return a
#' function:
#' 
#' \preformatted{function(x, w1 = NULL, w2 = NULL){...}}
#' 
#' `update_weights()` and `scale_weights()` return a numeric vector
#' the same length as `x`.
#' @note Transmuting, factoring, and scaling weights will return a value that
#' is the same length as `x`, so any missing values in `x` or
#' `w`/`w1`/`w2` will return `NA`. Unless all values are
#' `NA`, however, the result for transmuting or factoring will still
#' satisfy the above identities when `na.rm = TRUE` in
#' [`generalized_mean()`][generalized_mean] and
#' [`nested_mean()`][nested_mean]. Similarly, the result of scaling
#' will sum to 1 when missing values are removed.
#' @seealso [generalized_mean()] for the generalized mean.
#' 
#' [nested_mean()] for the nested mean.
#' 
#' [extended_mean()] for the extended mean that underlies
#' `transmute_weights()`.
#' 
#' [contributions()] for calculating additive percent-change
#' contributions.
#' 
#' [grouped()] to make each of these functions operate on grouped
#' data.
#' @references Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#' 
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). *Producer Price
#' Index Manual: Theory and Practice*. International Monetary Fund.
#' 
#' Martin, S. (2021). A note on general decompositions for price indexes.
#' *Prices Analytical Series*, Statistics Canada catalogue no. 62F0014M.
#' Statistics Canada, Ottawa.
#' 
#' Reinsdorf, M. B., Diewert, W. E., and Ehemann, C. (2002). Additive
#' decompositions for Fisher, Törnqvist and geometric mean indexes.
#' *Journal of Economic and Social Measurement*, 28(1-2):51--61.
#' 
#' Sydsaeter, K., Strom, A., and Berck, P. (2005). *Economists'
#' Mathematical Manual* (4th edition). Springer.
#' @examples
#' 
#' x <- 1:3
#' y <- 4:6
#' w <- 3:1
#' 
#' #---- Transforming generalized means ----
#' 
#' # Calculate the geometric mean as an arithmetic mean and 
#' # harmonic mean by transmuting the weights
#' 
#' geometric_mean(x)
#' arithmetic_mean(x, transmute_weights(0, 1)(x))
#' harmonic_mean(x, transmute_weights(0, -1)(x))
#' 
#' # Transmuting the weights for a harmonic mean into those
#' # for an arithmetic mean is the same as using weights w / x
#' 
#' all.equal(
#'   scale_weights(transmute_weights(-1, 1)(x, w)),
#'   scale_weights(w / x)
#' )
#' 
#' # Works for nested means, too
#' 
#' w1 <- 3:1
#' w2 <- 1:3
#' 
#' fisher_mean(x, w1, w2)
#' 
#' arithmetic_mean(x, nested_transmute(0, c(1, -1), 1)(x, w1, w2))
#' arithmetic_mean(x, nested_transmute2(0, c(1, -1), 1)(x, w1, w2))
#' 
#' # Note that nested_transmute() has an invariance property
#' # not shared by nested_transmute2()
#' 
#' all.equal(
#'   nested_transmute(0, c(1, -1), 1)(x, w1, w2),
#'   transmute_weights(2, 1)(
#'     x, nested_transmute(0, c(1, -1), 2)(x, w1, w2)
#'   )
#' )
#' 
#' all.equal(
#'   nested_transmute2(0, c(1, -1), 1)(x, w1, w2),
#'   transmute_weights(2, 1)(
#'     x, nested_transmute2(0, c(1, -1), 2)(x, w1, w2)
#'   )
#' )
#' 
#' #---- Percent-change contributions ----
#' 
#' # Transmuted weights can be used to calculate percent-change 
#' # contributions for, e.g., a geometric price index
#' 
#' scale_weights(transmute_weights(0, 1)(x)) * (x - 1)
#' geometric_contributions(x) # the more convenient way
#' 
#' #---- Basket representation of a price index ----
#' 
#' # Any generalized-mean index can be represented as a basket-style 
#' # index by transmuting the weights, which is how some authors 
#' # define a price index (e.g., Sydsaeter et al., 2005, p. 174)
#' 
#' p1 <- 2:6
#' p0 <- 1:5
#' 
#' qs <- transmute_weights(-1, 1)(p1 / p0) / p0
#' all.equal(harmonic_mean(p1 / p0), sum(p1 * qs) / sum(p0 * qs))
#' 
#' #---- Factoring the product of generalized means ----
#' 
#' # Factor the harmonic mean by chaining the calculation
#' 
#' harmonic_mean(x * y, w)
#' harmonic_mean(x, w) * harmonic_mean(y, factor_weights(-1)(x, w))
#' 
#' # The common case of an arithmetic mean
#' 
#' arithmetic_mean(x * y, w)
#' arithmetic_mean(x, w) * arithmetic_mean(y, update_weights(x, w))
#' 
#' # In cases where x and y have the same order, Chebyshev's 
#' # inequality implies that the chained calculation is too small
#' 
#' arithmetic_mean(x * y, w) > 
#'   arithmetic_mean(x, w) * arithmetic_mean(y, w)
#' 
NULL



