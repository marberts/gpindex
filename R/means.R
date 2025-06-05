#' Generalized mean
#'
#' Calculate a weighted generalized mean.
#'
#' The function `generalized_mean()` returns a function to compute the
#' generalized mean of `x` with weights `w` and exponent `r`
#' (i.e., \eqn{\prod_{i = 1}^{n} x_{i}^{w_{i}}}{\prod x^w} when \eqn{r = 0} and
#' \eqn{\left(\sum_{i = 1}^{n} w_{i} x_{i}^{r}\right)^{1 / r}}{(\sum wx^r)^1/r}
#' otherwise). This is also called the power mean, Hölder mean, or \eqn{l_p}
#' mean; see Bullen (2003, p. 175) for details.
#'
#' The functions `arithmetic_mean()`, `geometric_mean()`, and
#' `harmonic_mean()` compute the arithmetic, geometric, and harmonic (or
#' subcontrary) means, also known as the Pythagorean means. These are the most
#' useful means for making price indexes, and correspond to setting
#' \code{r = 1}, \code{r = 0}, and \code{r = -1} in `generalized_mean()`.
#'
#' Both `x` and `w` should be strictly positive (and finite),
#' especially for the purpose of making a price index. This is not enforced,
#' but the results may not make sense if the generalized mean is not defined.
#' There are two exceptions to this.
#' 1. The convention in Hardy et al. (1952, p. 13) is used in cases where `x`
#' has zeros: the generalized mean is 0 whenever `w` is strictly positive and
#' `r` < 0. (The analogous convention holds whenever at least one element of `x`
#' is `Inf`: the generalized mean is `Inf` whenever `w` is strictly positive
#' and `r` > 0.)
#'
#' 2. Some authors let `w` be non-negative and sum to 1 (e.g., Sydsaeter
#' et al., 2005, p. 47). If `w` has zeros, then the corresponding element
#' of `x` has no impact on the mean whenever `x` is strictly
#' positive. Unlike [weighted.mean()], however,
#' zeros in `w` are not strong zeros, so infinite values in `x` will
#' propagate even if the corresponding elements of `w` are zero.
#'
#' The weights are scaled to sum to 1 to satisfy the definition of a
#' generalized mean. There are certain price indexes where the weights should
#' not be scaled (e.g., the Vartia-I index); use [sum()] for
#' these cases.
#'
#' The underlying calculation returned by `generalized_mean()` is mostly
#' identical to [weighted.mean()], with one
#' important exception: missing values in the weights are not treated
#' differently than missing values in `x`. Setting `na.rm = TRUE`
#' drops missing values in both `x` and `w`, not just `x`. This
#' ensures that certain useful identities are satisfied with missing values in
#' `x`. In most cases `arithmetic_mean()` is a drop-in replacement
#' for [weighted.mean()].
#'
#' @param r A finite number giving the order of the generalized mean.
#' @param x A strictly positive numeric vector.
#' @param w A strictly positive numeric vector of weights, the same length as
#'   `x`. The default is to equally weight each element of `x`.
#' @param na.rm Should missing values in `x` and `w` be removed? By
#'   default missing values in `x` or `w` return a missing value.
#'
#' @returns
#' `generalized_mean()` returns a function:
#'
#' \preformatted{function(x, w = NULL, na.rm = FALSE){...}}
#'
#' `arithmetic_mean()`, `geometric_mean()`, and
#' `harmonic_mean()` each return a numeric value for the generalized means
#' of order 1, 0, and -1.
#'
#' @note
#' `generalized_mean()` can be defined on the extended real line, so
#' that \code{r = -Inf / Inf} returns [min()]/[max()], to agree with the
#' definition in, e.g., Bullen (2003). This is not implemented, and `r`
#' must be finite.
#'
#' There are a number of existing functions for calculating *unweighted*
#' geometric and harmonic means, namely the `geometric.mean()` and
#' `harmonic.mean()` functions in the \pkg{psych} package, the
#' `geomean()` function in the \pkg{FSA} package, the `GMean()` and
#' `HMean()` functions in the \pkg{DescTools} package, and the
#' `geoMean()` function in the \pkg{EnvStats} package. Similarly, the
#' `ci_generalized_mean()` function in the \pkg{Compind} package
#' calculates an *unweighted* generalized mean.
#'
#' @seealso
#' [transmute_weights()] transforms the weights to turn a generalized
#' mean of order \eqn{r} into a generalized mean of order \eqn{s}.
#'
#' [factor_weights()] calculates the weights to factor a mean of
#' products into a product of means.
#'
#' [price_indexes] and [quantity_index()] for simple
#' wrappers that use `generalized_mean()` to calculate common indexes.
#'
#' [back_period()]/[base_period()] for a simple utility
#' function to turn prices in a table into price relatives.
#'
#' @references
#' Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
#' Springer Science+Business Media.
#'
#' Fisher, I. (1922). *The Making of Index Numbers*. Houghton Mifflin
#' Company.
#'
#' Hardy, G., Littlewood, J. E., and Polya, G. (1952). *Inequalities* (2nd
#' edition). Cambridge University Press.
#'
#' IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020).
#' *Consumer Price Index Manual: Concepts and Methods*.
#' International Monetary Fund.
#'
#' Lord, N. (2002). Does Smaller Spread Always Mean Larger Product?
#' *The Mathematical Gazette*, 86(506): 273-274.
#'
#' Sydsaeter, K., Strom, A., and Berck, P. (2005).
#' *Economists' Mathematical Manual* (4th edition). Springer.
#'
#' @examples
#' x <- 1:3
#' w <- c(0.25, 0.25, 0.5)
#'
#' # The dispersion between the arithmetic, geometric, and harmonic
#' # mean usually increases as the variance of 'x' increases.
#'
#' x <- c(1, 3, 5)
#' y <- c(2, 3, 4)
#'
#' var(x) > var(y)
#'
#' arithmetic_mean(x) - geometric_mean(x)
#' arithmetic_mean(y) - geometric_mean(y)
#'
#' geometric_mean(x) - harmonic_mean(x)
#' geometric_mean(y) - harmonic_mean(y)
#'
#' # But the dispersion between these means is only bounded by the
#' # variance (Bullen, 2003, p. 156).
#'
#' arithmetic_mean(x) - geometric_mean(x) >= 2 / 3 * var(x) / (2 * max(x))
#' arithmetic_mean(x) - geometric_mean(x) <= 2 / 3 * var(x) / (2 * min(x))
#'
#' # Example by Lord (2002) where the dispersion decreases as the variance
#' # increases, counter to the claims by Fisher (1922, p. 108) and the
#' # CPI manual (par. 1.14)
#'
#' x <- (5 + c(sqrt(5), -sqrt(5), -3)) / 4
#' y <- (16 + c(7 * sqrt(2), -7 * sqrt(2), 0)) / 16
#'
#' var(x) > var(y)
#'
#' arithmetic_mean(x) - geometric_mean(x)
#' arithmetic_mean(y) - geometric_mean(y)
#'
#' geometric_mean(x) - harmonic_mean(x)
#' geometric_mean(y) - harmonic_mean(y)
#'
#' # The "bias" in the arithmetic and harmonic indexes is also smaller in
#' # this case, counter to the claim by Fisher (1922, p. 108)
#'
#' arithmetic_mean(x) * arithmetic_mean(1 / x) - 1
#' arithmetic_mean(y) * arithmetic_mean(1 / y) - 1
#'
#' harmonic_mean(x) * harmonic_mean(1 / x) - 1
#' harmonic_mean(y) * harmonic_mean(1 / y) - 1
#'
#' @family means
#' @export
generalized_mean <- function(r) {
  r <- as.numeric(r)
  if (not_finite_scalar(r)) {
    stop("'r' must be a finite length 1 numeric")
  }

  function(x, w = NULL, na.rm = FALSE) {
    if (is.null(w)) {
      if (na.rm && anyNA(x)) {
        x <- x[!is.na(x)]
      }
      if (r == 0) {
        exp(sum(log(x)) / length(x))
      } else if (r == 1) {
        # The arithmetic case is important enough for the optimization.
        sum(x) / length(x)
      } else {
        (sum(x^r) / length(x))^(1 / r)
      }
    } else {
      if (length(x) != length(w)) {
        stop("'x' and 'w' must be the same length")
      }
      if (na.rm && (anyNA(x) || anyNA(w))) {
        keep <- !(is.na(x) | is.na(w))
        x <- x[keep]
        w <- w[keep]
      }
      if (r == 0) {
        exp(sum(log(x) * w) / sum(w))
      } else if (r == 1) {
        sum(x * w) / sum(w)
      } else {
        (sum(x^r * w) / sum(w))^(1 / r)
      }
    }
  }
}

#' Arithmetic mean
#' @rdname generalized_mean
#' @export
arithmetic_mean <- generalized_mean(1)

#' Geometric mean
#' @rdname generalized_mean
#' @export
geometric_mean <- generalized_mean(0)

#' Harmonic mean
#' @rdname generalized_mean
#' @export
harmonic_mean <- generalized_mean(-1)

#' Extended mean
#'
#' Calculate a generalized logarithmic mean / extended mean.
#'
#' The function `extended_mean()` returns a function to compute the
#' component-wise extended mean of `a` and `b` of orders `r` and
#' `s`. See Bullen (2003, p. 393) for a definition. This is also called
#' the difference mean, Stolarsky mean, or extended mean-value mean.
#'
#' The function `generalized_logmean()` returns a function to compute the
#' component-wise generalized logarithmic mean of `a` and `b` of
#' order `r`. See Bullen (2003, p. 385) for a definition. The generalized
#' logarithmic mean is a special case of the extended mean, corresponding to
#' `extended_mean(r, 1)()`, but is more commonly used for price indexes.
#'
#' The function `logmean()` returns the ordinary component-wise
#' logarithmic mean of `a` and `b`, and corresponds to
#' `generalized_logmean(1)()`.
#'
#' Both `a` and `b` should be strictly positive. This is not
#' enforced, but the results may not make sense when the generalized
#' logarithmic mean / extended mean is not defined. The usual recycling rules
#' apply when `a` and `b` are not the same length.
#'
#' By definition, the generalized logarithmic mean / extended mean of `a`
#' and `b` is `a` when `a == b`. The `tol` argument is used
#' to test equality by checking if `abs(a - b) <= tol`. The default value
#' is the same as [all.equal()]. In some cases it's useful to multiply
#' `tol` by a scale factor, such as `max(abs(a), abs(b))`. This often
#' doesn't matter when making price indexes, however, as `a` and `b`
#' are usually around 1.
#'
#' @param r,s A finite number giving the order of the generalized logarithmic
#' mean / extended mean.
#' @param a,b A strictly positive numeric vector.
#' @param tol The tolerance used to determine if `a == b`.
#'
#' @returns
#' `generalized_logmean()` and `extended_mean()` return a
#' function:
#'
#' \preformatted{function(a, b, tol = .Machine$double.eps^0.5){...}}
#'
#' `logmean()` returns a numeric vector, the same length as
#' `max(length(a), length(b))`, giving the component-wise logarithmic mean
#' of `a` and `b`.
#'
#' @note
#' `generalized_logmean()` can be defined on the extended real line,
#' so that \code{r = -Inf / Inf} returns [pmin()]/[pmax()], to agree with the
#' definition in, e.g., Bullen (2003). This is not implemented, and `r`
#' must be finite.
#'
#' @seealso
#' [transmute_weights()] uses the extended mean to turn a generalized
#' mean of order \eqn{r} into a generalized mean of order \eqn{s}.
#'
#' @references
#' Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
#' Springer Science+Business Media.
#'
#' @examples
#' x <- 8:5
#' y <- 1:4
#'
#' # The arithmetic and geometric means are special cases of the
#' # generalized logarithmic mean.
#'
#' all.equal(generalized_logmean(2)(x, y), (x + y) / 2)
#' all.equal(generalized_logmean(-1)(x, y), sqrt(x * y))
#'
#' # The harmonic mean cannot be expressed as a logarithmic mean, but can
#' # be expressed as an extended mean.
#'
#' all.equal(extended_mean(-2, -1)(x, y), 2 / (1 / x + 1 / y))
#'
#' # The quadratic mean is also a type of extended mean.
#'
#' all.equal(extended_mean(2, 4)(x, y), sqrt(x^2 / 2 + y^2 / 2))
#'
#' # As are heronian and centroidal means.
#'
#' all.equal(
#'   extended_mean(0.5, 1.5)(x, y),
#'   (x + sqrt(x * y) + y) / 3
#' )
#' all.equal(
#'   extended_mean(2, 3)(x, y),
#'   2 / 3 * (x^2 + x * y + y^2) / (x + y)
#' )
#'
#' @family means
#' @export
extended_mean <- function(r, s) {
  r <- as.numeric(r)
  s <- as.numeric(s)
  if (not_finite_scalar(r)) {
    stop("'r' must be a finite length 1 numeric")
  }
  if (not_finite_scalar(s)) {
    stop("'s' must be a finite length 1 numeric")
  }

  function(a, b, tol = .Machine$double.eps^0.5) {
    if (length(tol) > max(length(a), length(b))) {
      stop("'tol' cannot be longer than 'a' or 'b'")
    }
    if (r == 0 && s == 0) {
      res <- sqrt(a * b)
    } else if (r == 0) {
      res <- ((a^s - b^s) / log(a / b) / s)^(1 / s)
    } else if (s == 0) {
      res <- ((a^r - b^r) / log(a / b) / r)^(1 / r)
    } else if (r == s) {
      res <- exp((a^r * log(a) - b^r * log(b)) / (a^r - b^r) - 1 / r)
    } else {
      res <- ((a^s - b^s) / (a^r - b^r) * (r / s))^(1 / (s - r))
    }
    # Set output to a when a == b.
    i <- which(abs(a - b) <= tol)
    res[i] <- a[(i - 1L) %% length(a) + 1L]
    res
  }
}

#' Generalized logarithmic mean
#' @rdname extended_mean
#' @export
generalized_logmean <- function(r) {
  extended_mean(r, 1)
}

#' Logarithmic mean
#' @rdname extended_mean
#' @export
logmean <- generalized_logmean(0)

#' Lehmer mean
#'
#' Calculate a weighted Lehmer mean.
#'
#' The function `lehmer_mean()` returns a function to compute the Lehmer
#' mean of order `r` of `x` with weights `w`, which is
#' calculated as the arithmetic mean of `x` with weights \eqn{wx^{r-1}}.
#' This is also called the counter-harmonic mean or generalized anti-harmonic
#' mean. See Bullen (2003, p. 245) for details.
#'
#' The Lehmer mean of order 2 is sometimes called the contraharmonic (or
#' anti-harmonic) mean. The function `contraharmonic_mean()` simply calls
#' `lehmer_mean(2)()`. See von der Lippe
#' (2015) for more details on the use of these means for making price indexes.
#'
#' @inheritParams generalized_mean
#' @param r A finite number giving the order of the Lehmer mean.
#'
#' @returns
#' `lehmer_mean()` returns a function:
#'
#' \preformatted{function(x, w = NULL, na.rm = FALSE){...}}
#'
#' `contraharmonic_mean()` returns a numeric value for the Lehmer mean of
#' order 2.
#'
#' @note
#' `lehmer_mean()` can be defined on the extended real line, so that
#' \code{r = -Inf / Inf} returns [min()]/[max()], to agree with the
#' definition in, e.g., Bullen (2003). This is not implemented, and `r`
#' must be finite.
#'
#' @references
#' Bullen, P. S. (2003). *Handbook of Means and Their Inequalities*.
#' Springer Science+Business Media.
#'
#' von der Lippe, P. (2015). Generalized Statistical Means and New Price Index
#' Formulas, Notes on some unexplored index formulas, their interpretations and
#' generalizations. Munich Personal RePEc Archive paper no. 64952.
#'
#' @examples
#' x <- 2:3
#' w <- c(0.25, 0.75)
#'
#' # The Pythagorean means are special cases of the Lehmer mean.
#'
#' all.equal(lehmer_mean(1)(x, w), arithmetic_mean(x, w))
#' all.equal(lehmer_mean(0)(x, w), harmonic_mean(x, w))
#' all.equal(lehmer_mean(0.5)(x), geometric_mean(x))
#'
#' # When r < 1, the generalized mean is larger than the corresponding
#' # Lehmer mean.
#'
#' lehmer_mean(-1)(x, w) < generalized_mean(-1)(x, w)
#'
#' # The reverse is true when r > 1.
#'
#' lehmer_mean(3)(x, w) > generalized_mean(3)(x, w)
#'
#' # This implies the contraharmonic mean is larger than the quadratic
#' # mean, and therefore the Pythagorean means.
#'
#' contraharmonic_mean(x, w) > arithmetic_mean(x, w)
#' contraharmonic_mean(x, w) > geometric_mean(x, w)
#' contraharmonic_mean(x, w) > harmonic_mean(x, w)
#'
#' # ... and the logarithmic mean
#'
#' contraharmonic_mean(2:3) > logmean(2, 3)
#'
#' # The difference between the arithmetic mean and contraharmonic mean
#' # is proportional to the variance of x.
#'
#' weighted_var <- function(x, w) {
#'   arithmetic_mean((x - arithmetic_mean(x, w))^2, w)
#' }
#'
#' arithmetic_mean(x, w) + weighted_var(x, w) / arithmetic_mean(x, w)
#' contraharmonic_mean(x, w)
#'
#' @family means
#' @export
lehmer_mean <- function(r) {
  r <- as.numeric(r)
  if (not_finite_scalar(r)) {
    stop("'r' must be a finite length 1 numeric")
  }

  function(x, w = NULL, na.rm = FALSE) {
    v <- x^(r - 1)
    if (!is.null(w)) {
      if (length(x) != length(w)) {
        stop("'x' and 'w' must be the same length")
      }
      v <- v * w
    }
    arithmetic_mean(x, v, na.rm = na.rm)
  }
}

#' Contraharmonic mean
#' @rdname lehmer_mean
#' @export
contraharmonic_mean <- lehmer_mean(2)

#' Nested generalized mean
#'
#' Calculate the (outer) generalized mean of two (inner) generalized means
#' (i.e., crossing generalized means).
#'
#' @param r1 A finite number giving the order of the outer generalized mean.
#' @param r2 A pair of finite numbers giving the order of the inner generalized
#' means.
#' @param t A pair of strictly positive weights for the inner generalized
#' means. The default is equal weights.
#' @param x A strictly positive numeric vector.
#' @param w1,w2 A strictly positive numeric vector of weights, the same length
#' as `x`. The default is to equally weight each element of `x`.
#' @param na.rm Should missing values in `x`, `w1`, and `w2` be
#' removed? By default missing values in `x`, `w1`, or `w2`
#' return a missing value.
#'
#' @returns
#' `nested_mean()` returns a function:
#'
#' \preformatted{function(x, w1 = NULL, w2 = NULL, na.rm = FALSE){...}}
#'
#' This computes the generalized mean of order `r1` of the generalized
#' mean of order `r2[1]` of `x` with weights `w1` and the
#' generalized mean of order `r2[2]` of `x` with weights `w2`.
#'
#' `fisher_mean()` returns a numeric value for the geometric mean of the
#' arithmetic and harmonic means (i.e., `r1 = 0` and `r2 = c(1, -1)`).
#'
#' @note
#' There is some ambiguity about how to remove missing values in
#' `w1` or `w2` when `na.rm = TRUE`. The approach here is to
#' remove missing values when calculating each of the inner means individually,
#' rather than removing all missing values prior to any calculations. This
#' means that a different number of data points could be used to calculate the
#' inner means. Use the [balanced()] operator to balance
#' missing values across `w1` and `w2` prior to any calculations.
#'
#' @seealso
#' [nested_contributions()] for percent-change contributions for
#' indexes based on nested generalized means, like the Fisher index.
#'
#' @references
#' ILO, IMF, OECD, UNECE, and World Bank. (2004).
#' *Producer Price Index Manual: Theory and Practice*. International Monetary
#' Fund.
#'
#' @examples
#' x <- 1:3
#' w1 <- 4:6
#' w2 <- 7:9
#'
#' # A function to make the superlative quadratic mean price index as
#' # a product of generalized means.
#'
#' quadratic_mean_index <- function(r) nested_mean(0, c(r / 2, -r / 2))
#'
#' quadratic_mean_index(2)(x, w1, w2)
#'
#' fisher_mean(x, w1, w2)
#'
#' # The (arithmetic) Walsh index is the implicit price index when using a
#' # superlative quadratic mean quantity index of order 1.
#'
#' p2 <- price6[[2]]
#' p1 <- price6[[1]]
#' q2 <- quantity6[[2]]
#' q1 <- quantity6[[1]]
#'
#' walsh <- quadratic_mean_index(1)
#'
#' sum(p2 * q2) / sum(p1 * q1) / walsh(q2 / q1, p1 * q1, p2 * q2)
#'
#' sum(p2 * sqrt(q2 * q1)) / sum(p1 * sqrt(q2 * q1))
#'
#' # Counter to the PPI manual (par. 1.105), it is not a superlative
#' # quadratic mean price index of order 1.
#'
#' walsh(p2 / p1, p1 * q1, p2 * q2)
#'
#' # That requires using the average value share as weights.
#'
#' walsh_weights <- sqrt(scale_weights(p1 * q1) * scale_weights(p2 * q2))
#' walsh(p2 / p1, walsh_weights, walsh_weights)
#'
#' @family means
#' @export
nested_mean <- function(r1, r2, t = c(1, 1)) {
  outer_mean <- generalized_mean(r1)

  r2 <- as.numeric(r2)
  if (not_finite_pair(r2)) {
    stop("'r2' must be a pair of finite numeric values")
  }

  inner_mean1 <- generalized_mean(r2[1L])
  inner_mean2 <- generalized_mean(r2[2L])

  t <- as.numeric(t)
  if (length(t) != 2) {
    stop("'t' must be a pair of numeric values")
  }

  function(x, w1 = NULL, w2 = NULL, na.rm = FALSE) {
    x <- c(inner_mean1(x, w1, na.rm), inner_mean2(x, w2, na.rm))
    outer_mean(x, t, na.rm)
  }
}

#' Fisher mean
#' @rdname nested_mean
#' @export
fisher_mean <- nested_mean(0, c(1, -1))
