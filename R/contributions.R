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
#' The `arithmetic_contributions()`, `geometric_contributions()` and
#' `harmonic_contributions()` functions cover the most important cases
#' (i.e., \code{r = 1}, \code{r = 0}, and \code{r = -1}).
#'
#' The `nested_contributions()` and `nested_contributions2()`
#' functions are the analog of `contributions()` for an index based on a
#' nested generalized mean with two levels, like a Fisher index. They are
#' wrappers around `nested_transmute()` and `nested_transmute2()`, respectively.
#'
#' The `fisher_contributions()` and `fisher_contributions2()`
#' functions correspond to `nested_contributions(0, c(1, -1))()` and
#' `nested_contributions2(0, c(1, -1))()`, and are appropriate for
#' calculating percent-change contributions for a Fisher index.
#'
#' @inheritParams generalized_mean
#' @inheritParams nested_mean
#' @param w,w1,w2 A strictly positive numeric vector of weights, the same length
#'   as `x`. The default is to equally weight each element of `x`.
#'
#' @returns
#' `contributions()` returns a function:
#'
#' \preformatted{function(x, w = NULL){...}}
#'
#' `nested_contributions()` and `nested_contributions2()` return a
#' function:
#'
#' \preformatted{function(x, w1 = NULL, w2 = NULL){...}}
#'
#' `arithmetic_contributions()`, `geometric_contributions()`,
#' `harmonic_contributions()`, `fisher_contributions()`, and
#' `fisher_contributions2()` each return a numeric vector, the same
#' length as `x`.
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
#' Reinsdorf, M. B., Diewert, W. E., and Ehemann, C. (2002). Additive
#' decompositions for Fisher, Törnqvist and geometric mean indexes.
#' *Journal of Economic and Social Measurement*, 28(1-2):51--61.
#'
#' @examples
#' p2 <- price6[[2]]
#' p1 <- price6[[1]]
#' q2 <- quantity6[[2]]
#' q1 <- quantity6[[1]]
#'
#' # Percent-change contributions for the Jevons index.
#'
#' geometric_mean(p2 / p1) - 1
#'
#' geometric_contributions(p2 / p1)
#'
#' all.equal(
#'   geometric_mean(p2 / p1) - 1,
#'   sum(geometric_contributions(p2 / p1))
#' )
#'
#' # Percent-change contributions for the Fisher index in section 6 of
#' # Reinsdorf et al. (2002).
#'
#' (con <- fisher_contributions(p2 / p1, p1 * q1, p2 * q2))
#'
#' all.equal(sum(con), fisher_index(p2, p1, q2, q1) - 1)
#'
#' # Not the only way.
#'
#' (con2 <- fisher_contributions2(p2 / p1, p1 * q1, p2 * q2))
#'
#' all.equal(sum(con2), fisher_index(p2, p1, q2, q1) - 1)
#'
#' # The same as the van IJzeren decomposition in section 4.2.2 of
#' # Balk (2008).
#'
#' Qf <- quantity_index(fisher_index)(q2, q1, p2, p1)
#' Ql <- quantity_index(laspeyres_index)(q2, q1, p1)
#' wl <- scale_weights(p1 * q1)
#' wp <- scale_weights(p1 * q2)
#'
#' (Qf / (Qf + Ql) * wl + Ql / (Qf + Ql) * wp) * (p2 / p1 - 1)
#'
#' # Similar to the method in section 2 of Reinsdorf et al. (2002),
#' # although those contributions aren't based on weights that sum to 1.
#'
#' Pf <- fisher_index(p2, p1, q2, q1)
#' Pl <- laspeyres_index(p2, p1, q1)
#'
#' (1 / (1 + Pf) * wl + Pl / (1 + Pf) * wp) * (p2 / p1 - 1)
#'
#' # Also similar to the decomposition by Hallerbach (2005), noting that
#' # the Euler weights are close to unity.
#'
#' Pp <- paasche_index(p2, p1, q2)
#'
#' (0.5 * sqrt(Pp / Pl) * wl + 0.5 * sqrt(Pl / Pp) * wp) * (p2 / p1 - 1)
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
