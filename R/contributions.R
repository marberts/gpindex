#' Percent-change contributions
#'
#' Calculate additive percent-change contributions for generalized-mean price
#' indexes, and indexes that nest two levels of generalized means consisting of
#' an outer generalized mean and two inner generalized means (e.g., the Fisher
#' index).
#'
#' The function `contributions()` is a simple wrapper for
#' [`transmute_weights(r, 1)()`][transmute_weights] to calculate
#' (additive) percent-change contributions for a price index based on a
#' generalized mean of order `r`. It returns a function to compute a
#' vector `v(x, w)` such that
#'
#' \preformatted{generalized_mean(r)(x, w) - 1 == sum(v(x, w))}
#'
#' This generalizes the approach for calculating percent-change contributions
#' in section 4.2 of Balk (2008) using the method by Martin (2021). The
#' `arithmetic_contributions()`, `geometric_contributions()` and
#' `harmonic_contributions()` functions cover the most important cases
#' (i.e., \code{r = 1}, \code{r = 0}, and \code{r = -1}).
#'
#' The `nested_contributions()` and `nested_contributions2()`
#' functions are the analog of `contributions()` for an index based on a
#' nested generalized mean with two levels, like a Fisher index. They return a
#' function that calculates the contribution of each element of `x` when a
#' generalized mean of order `r1` aggregates two generalized-mean indexes
#' of `x` with orders `r2`, and weights `w1` and `w2`.
#'
#' Unlike the case of a generalized-mean index, there are several ways to make
#' contributions for an index based on nested generalized means.
#' `nested_contributions()` uses a generalization of the algorithm in
#' section 6 of Reinsdorf et al. (2002) by Martin (2021).
#' `nested_contributions2()` generalizes the van IJzeren decomposition for
#' the Fisher index (Balk, 2008, section 4.2.2) by constructing a weighted
#' average of the contributions for both of the inner means with the approach
#' by Martin (2021). In most cases the results are broadly similar.
#'
#' The `fisher_contributions()` and `fisher_contributions2()`
#' functions correspond to `nested_contributions(0, c(1, -1))()` and
#' `nested_contributions2(0, c(1, -1))()`, and are appropriate for
#' calculating percent-change contributions for a Fisher index.
#'
#' @inheritParams generalized_mean
#' @inheritParams nested_mean
#' @param w,w1,w2 A strictly positive numeric vector of weights, the same length
#' as `x`. The default is to equally weight each element of `x`.
#'
#' @returns
#' `contributions()` returns a function:
#'
#' \preformatted{function(x, w = NULL){...}}
#'
#' This computes the additive contribution for each element of `x` in an
#' index based on the generalized mean of order `r` with weights `w`.
#'
#' `nested_contributions()` and `nested_contributions2()` return a
#' function:
#'
#' \preformatted{function(x, w1 = NULL, w2 = NULL){...}}
#'
#' This computes the additive contribution for each element of `x` when a
#' generalized mean of order `r1` aggregates a generalized-mean index of
#' order `r2[1]` with weights `w1` and a generalized-mean index of
#' order `r2[2]` with weights `w2`.
#'
#' `arithmetic_contributions()`, `geometric_contributions()`, and
#' `harmonic_contributions()` each return a numeric vector, the same
#' length as `x`, giving the contribution of each element of `x` in
#' an arithmetic, geometric, or harmonic index.
#'
#' `fisher_contributions()` and `fisher_contributions2()` each return
#' a numeric vector, the same length as `x`, giving the contribution of
#' each element of `x` when a geometric mean aggregates an arithmetic mean
#' of `x` with weights `w1` and a harmonic mean of `x` with
#' weights `w2`.
#'
#' @seealso
#' [transmute_weights()] for the underlying implementation.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' Hallerbach, W. G. (2005). An alternative decomposition of the Fisher index.
#' *Economics Letters*, 86(2):147--152
#'
#' Martin, S. (2021). A note on general decompositions for price indexes.
#' *Prices Analytical Series*, Statistics Canada catalogue no. 62F0014M.
#' Statistics Canada, Ottawa.
#'
#' Reinsdorf, M. B., Diewert, W. E., and Ehemann, C. (2002). Additive
#' decompositions for Fisher, Törnqvist and geometric mean indexes.
#' *Journal of Economic and Social Measurement*, 28(1-2):51--61.
#'
#' Webster, M. and Tarnow-Mordi, R. C. (2019). Decomposing multilateral price
#' indexes into the contributions of individual commodities.
#' *Journal of Official Statistics*, 35(2):461--486.
#'
#' @examples
#' x <- 2:3
#'
#' #---- Contributions for a geometric index ----
#'
#' geometric_mean(x) - 1 # percent change in the Jevons index
#'
#' geometric_contributions(x)
#'
#' all.equal(geometric_mean(x) - 1, sum(geometric_contributions(x)))
#'
#' # This works by first transmuting the weights in the geometric mean
#' # into weights for an arithmetic mean, then finding the contributions
#' # to the percent change
#'
#' transmute_weights(0, 1)(x) * (x - 1)
#'
#' # Not the only way to calculate contributions
#'
#' transmute2 <- function(x) {
#'   m <- geometric_mean(x)
#'   (m - 1) / log(m) * log(x) / (x - 1) / length(x)
#' }
#'
#' transmute2(x) * (x - 1) # not proportional to the method above
#' all.equal(sum(transmute2(x) * (x - 1)), geometric_mean(x) - 1)
#'
#' # But these "transmuted" weights don't recover the geometric mean!
#' # Not a particularly good way to calculate contributions
#'
#' isTRUE(all.equal(
#'   arithmetic_mean(x, transmute2(x)),
#'   geometric_mean(x)
#' ))
#'
#' # There are infinitely many ways to calculate contributions, but the
#' # weights from transmute_weights(0, 1)() are the *unique* weights that
#' # recover the geometric mean
#'
#' perturb <- function(w, e) {
#'   w + c(e, -e) / (x - 1)
#' }
#'
#' perturb(transmute2(x), 0.1) * (x - 1)
#' all.equal(
#'   sum(perturb(transmute2(x), 0.1) * (x - 1)),
#'   geometric_mean(x) - 1
#' )
#' isTRUE(all.equal(
#'   arithmetic_mean(x, perturb(transmute2(x), 0.1)),
#'   geometric_mean(x)
#' ))
#'
#' #---- Contributions for a Fisher index ----
#'
#' p1 <- price6[[2]]
#' p0 <- price6[[1]]
#' q1 <- quantity6[[2]]
#' q0 <- quantity6[[1]]
#'
#' # Percent-change contributions for the Fisher index in section 6 of
#' # Reinsdorf et al. (2002)
#'
#' (con <- fisher_contributions(
#'   p1 / p0,
#'   index_weights("Laspeyres")(p0, q0),
#'   index_weights("Paasche")(p1, q1)
#' ))
#'
#' all.equal(sum(con), fisher_index(p1, p0, q1, q0) - 1)
#'
#' # Not the only way
#'
#' (con2 <- fisher_contributions2(
#'   p1 / p0,
#'   index_weights("Laspeyres")(p0, q0),
#'   index_weights("Paasche")(p1, q1)
#' ))
#'
#' all.equal(sum(con2), fisher_index(p1, p0, q1, q0) - 1)
#'
#' # The same as the van IJzeren decomposition in section 4.2.2 of
#' # Balk (2008)
#'
#' Qf <- quantity_index(fisher_index)(q1, q0, p1, p0)
#' Ql <- quantity_index(laspeyres_index)(q1, q0, p0)
#' wl <- scale_weights(index_weights("Laspeyres")(p0, q0))
#' wp <- scale_weights(index_weights("HybridPaasche")(p0, q1))
#'
#' (Qf / (Qf + Ql) * wl + Ql / (Qf + Ql) * wp) * (p1 / p0 - 1)
#'
#' # Similar to the method in section 2 of Reinsdorf et al. (2002),
#' # although those contributions aren't based on weights that sum to 1
#'
#' Pf <- fisher_index(p1, p0, q1, q0)
#' Pl <- laspeyres_index(p1, p0, q0)
#'
#' (1 / (1 + Pf) * wl + Pl / (1 + Pf) * wp) * (p1 / p0 - 1)
#'
#' # Also similar to the decomposition by Hallerbach (2005), noting that
#' # the Euler weights are close to unity
#'
#' Pp <- paasche_index(p1, p0, q1)
#'
#' (0.5 * sqrt(Pp / Pl) * wl + 0.5 * sqrt(Pl / Pp) * wp) * (p1 / p0 - 1)
#'
#' #---- Contributions for other types of indexes ----
#'
#' # A function to get contributions for any superlative quadratic mean of
#' # order 'r' index
#'
#' superlative_contributions <- function(r) {
#'   nested_contributions(0, c(r / 2, -r / 2))
#' }
#' 
#' # Can be used to decompose the implict Walsh index
#' 
#' superlative_contributions(1)(
#'   p1 / p0,
#'   index_weights("Laspeyres")(p0, q0),
#'   index_weights("Paasche")(p1, q1)
#' )
#'
#' # Works for other types of indexes, like the harmonic
#' # Laspeyres Paasche index
#'
#' hlp_contributions <- nested_contributions(-1, c(1, -1))
#' hlp_contributions(
#'   p1 / p0,
#'   index_weights("Laspeyres")(p0, q0),
#'   index_weights("Paasche")(p1, q1)
#' )
#'
#' # Or the AG mean index (tau = 0.25)
#'
#' agmean_contributions <- nested_contributions(1, c(0, 1), c(0.25, 0.75))
#' agmean_contributions(
#'   p1 / p0,
#'   index_weights("Laspeyres")(p0, q0),
#'   index_weights("Laspeyres")(p0, q0)
#' )
#'
#' # Or the Balk-Walsh index
#'
#' bw_contributions <- nested_contributions(0, c(0.5, -0.5))
#' bw_contributions(p1 / p0)
#'
#' @export
contributions <- function(r) {
  arithmetic_weights <- transmute_weights(r, 1)

  function(x, w = NULL) {
    (x - 1) * arithmetic_weights(x, w)
  }
}

#' Arithmetic contributions
#' @rdname contributions
#' @export
arithmetic_contributions <- contributions(1)

#' Geometric contributions
#' @rdname contributions
#' @export
geometric_contributions <- contributions(0)

#' Harmonic contributions
#' @rdname contributions
#' @export
harmonic_contributions <- contributions(-1)

#' Factory to make nested contributions factory
#' @noRd
nc <- function(nest_transmute) {
  nest_transmute <- match.fun(nest_transmute)

  function(r1, r2, t = c(1, 1)) {
    arithmetic_weights <- nest_transmute(r1, r2, 1, t)

    function(x, w1 = NULL, w2 = NULL) {
      (x - 1) * arithmetic_weights(x, w1, w2)
    }
  }
}

#' Nested contributions
#' @rdname contributions
#' @export
nested_contributions <- nc(nested_transmute)

#' Nested contributions, take 2
#' @rdname contributions
#' @export
nested_contributions2 <- nc(nested_transmute2)

#' Fisher contributions
#' @rdname contributions
#' @export
fisher_contributions <- nested_contributions(0, c(1, -1))

#' Fisher contributions, take 2
#' @rdname contributions
#' @export
fisher_contributions2 <- nested_contributions2(0, c(1, -1))
