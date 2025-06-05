#' Simplified extended mean for transmuting weights
#' @noRd
rdiff <- function(a, b, r) {
  if (r == 0) {
    log(a / b)
  } else if (r == 1) {
    a - b
  } else {
    (a^r - b^r) / r
  }
}

extended_mean_ <- function(r, s) {
  r <- as.numeric(r)
  s <- as.numeric(s)
  if (not_finite_scalar(r)) {
    stop("'r' must be a finite length 1 numeric")
  }
  if (not_finite_scalar(s)) {
    stop("'s' must be a finite length 1 numeric")
  }

  function(x, m, tol) {
    res <- rdiff(x, m, r) / rdiff(x, m, s)
    res[abs(x - m) <= tol] <- m^(r - s)
    res
  }
}

#' Transmute weights
#'
#' Transmute weights to turn a generalized mean of order \eqn{r} into a
#' generalized mean of order \eqn{s}. Useful for calculating additive and
#' multiplicative decompositions for a generalized-mean index, and those made
#' of nested generalized means (e.g., Fisher index).
#'
#' The function `transmute_weights(r, s)` returns a function to compute a
#' vector of weights `v(x, w)` such that
#'
#' \preformatted{generalized_mean(r)(x, w) == generalized_mean(s)(x, v(x, w))}
#'
#' `nested_transmute(r1, r2, t, s)` and `nested_transmute2(r1, r2, t, s)` do
#' the same for nested generalized means, so that
#'
#' \preformatted{nested_mean(r1, r2, t)(x, w1, w2) ==
#'   generalized_mean(s)(x, v(x, w1, w2))}
#'
#' Transmuting weights returns a value that is the same length as \code{x},
#' so any missing values in \code{x} or the weights will return \code{NA}.
#' Unless all values are \code{NA}, however, the result will still satisfy
#' the above identities when \code{na.rm = TRUE}.
#'
#' @inheritParams nested_mean
#' @param r,s A finite number giving the order of the generalized mean. See
#'   details.
#'
#' @returns
#' `transmute_weights()` returns a function:
#'
#' \preformatted{function(x, w = NULL, tol = .Machine$double.eps^0.5){...}}
#'
#' `nested_transmute()` and `nested_transmute2()` similarly return a
#' function:
#'
#' \preformatted{function(x, w1 = NULL, w2 = NULL, tol = .Machine$double.eps^0.5){...}}
#'
#' @seealso
#' [generalized_mean()] for the generalized mean and [nested_mean()] for the
#' nested mean.
#'
#' [extended_mean()] for the extended mean that underlies
#' `transmute_weights()`.
#'
#' [contributions()] for calculating additive percent-change
#' contributions.
#'
#' [grouped()] to make these functions operate on grouped data.
#'
#' @references
#' See `vignette("decomposing-indexes")` for more details.
#'
#' @examples
#' x <- 1:3
#' w <- 3:1
#'
#' # Calculate the geometric mean as an arithmetic mean and
#' # harmonic mean by transmuting the weights.
#'
#' geometric_mean(x)
#' arithmetic_mean(x, transmute_weights(0, 1)(x))
#' harmonic_mean(x, transmute_weights(0, -1)(x))
#'
#' # Works for nested means, too.
#'
#' w1 <- 3:1
#' w2 <- 1:3
#'
#' fisher_mean(x, w1, w2)
#'
#' arithmetic_mean(x, nested_transmute(0, c(1, -1), 1)(x, w1, w2))
#' arithmetic_mean(x, nested_transmute2(0, c(1, -1), 1)(x, w1, w2))
#' @family weights functions
#' @export
transmute_weights <- function(r, s) {
  r <- as.numeric(r)
  s <- as.numeric(s)
  gen_mean <- generalized_mean(r)
  pow_ext_mean <- extended_mean_(r, s)

  function(x, w = NULL, tol = .Machine$double.eps^0.5) {
    if (r == s) {
      if (is.null(w)) {
        w <- rep.int(1, length(x))
      }
      if (length(x) != length(w)) {
        stop("'x' and 'w' must be the same length")
      }
      w[is.na(x)] <- NA_real_
      scale_weights(w)
    } else {
      if (length(tol) != 1L && length(tol) != length(x)) {
        stop("'tol' must be either length 1 or the same length as 'x'")
      }
      m <- gen_mean(x, w, na.rm = TRUE)
      if (is.null(w)) {
        v <- pow_ext_mean(x, m, tol)
        attributes(v) <- NULL
      } else {
        v <- w * pow_ext_mean(x, m, tol)
        attributes(v) <- attributes(w)
      }
      scale_weights(v)
    }
  }
}

#' Transmute the weights in a nested generalized mean
#' @rdname transmute_weights
#' @export
nested_transmute <- function(r1, r2, s, t = c(1, 1)) {
  s_weights <- transmute_weights(r1, s)

  r2 <- as.numeric(r2)
  if (not_finite_pair(r2)) {
    stop("'r2' must be a pair of finite numeric values")
  }

  r_weights1 <- transmute_weights(r2[1L], r1)
  r_weights2 <- transmute_weights(r2[2L], r1)

  t <- as.numeric(t)
  if (length(t) != 2L) {
    stop("'t' must be a pair of numeric values")
  }

  function(x, w1 = NULL, w2 = NULL, tol = .Machine$double.eps^0.5) {
    if (is.na(t[1L]) && !is.na(t[2L])) {
      w <- r_weights2(x, w2, tol)
    } else if (!is.na(t[1L]) && is.na(t[2L])) {
      w <- r_weights1(x, w1, tol)
    } else {
      v1 <- r_weights1(x, w1, tol)
      v2 <- r_weights2(x, w2, tol)
      # The calculation is wrong if NAs in w1 or w2 propagate.
      if (anyNA(w1)) {
        v1[is.na(v1) & !is.na(v2)] <- 0
      }
      if (anyNA(w2)) {
        v2[is.na(v2) & !is.na(v1)] <- 0
      }
      w <- t[1L] * v1 + t[2L] * v2
    }
    s_weights(x, w, tol)
  }
}

#' Transmute the weights in a nested generalized mean, take 2
#' @rdname transmute_weights
#' @export
nested_transmute2 <- function(r1, r2, s, t = c(1, 1)) {
  s_weights <- transmute_weights(r1, s)

  r2 <- as.numeric(r2)
  if (length(r2) != 2L) {
    stop("'r2' must be a pair of finite numeric values")
  }

  s_weights1 <- transmute_weights(r2[1L], s)
  s_weights2 <- transmute_weights(r2[2L], s)
  mean1 <- generalized_mean(r2[1L])
  mean2 <- generalized_mean(r2[2L])

  t <- as.numeric(t)
  if (length(t) != 2L) {
    stop("'t' must be a pair of numeric values")
  }

  function(x, w1 = NULL, w2 = NULL, tol = .Machine$double.eps^0.5) {
    m <- c(mean1(x, w1, na.rm = TRUE), mean2(x, w2, na.rm = TRUE))
    v <- s_weights(m, t, tol)
    if (is.na(v[1L]) && !is.na(v[2L])) {
      s_weights2(x, w2, tol)
    } else if (!is.na(v[1L]) && is.na(v[2L])) {
      s_weights1(x, w1, tol)
    } else {
      u1 <- s_weights1(x, w1, tol)
      u2 <- s_weights2(x, w2, tol)
      # The calculation is wrong if NAs in w1 or w2 propagate.
      if (anyNA(w1)) {
        u1[is.na(u1) & !is.na(u2)] <- 0
      }
      if (anyNA(w2)) {
        u2[is.na(u2) & !is.na(u1)] <- 0
      }
      v[1L] * u1 + v[2L] * u2
    }
  }
}

#' Factor weights
#'
#' Factor weights to turn the generalized mean of a product into the product
#' of generalized means. Useful for price-updating the weights in a
#' generalized-mean index.
#'
#' The function `factor_weights(r)` returns a function to compute weights
#' `u(x, w)` such that
#'
#' \preformatted{generalized_mean(r)(x * y, w) ==
#'   generalized_mean(r)(x, w) * generalized_mean(r)(y, u(x, w))}
#'
#' This generalizes the result in section C.5 of Chapter 9 of the PPI Manual
#' for chaining the Young index, and gives a way to chain generalized-mean
#' price indexes over time.
#'
#' Factoring weights with \code{r = 1} sometimes gets called price-updating
#' weights; `update_weights()` simply calls `factor_weights(1)()`.
#'
#' Factoring weights return a value that is the same length as \code{x},
#' so any missing values in \code{x} or the weights will return \code{NA}.
#' Unless all values are \code{NA}, however, the result will still satisfy
#' the above identity when \code{na.rm = TRUE}.
#'
#' @inheritParams generalized_mean
#'
#' @returns
#' `factor_weights()` return a function:
#'
#' \preformatted{function(x, w = NULL){...}}
#'
#' `update_weights()` returns a numeric vector the same length as `x`.
#'
#' @seealso
#' [generalized_mean()] for the generalized mean.
#'
#' [grouped()] to make these functions operate on grouped data.
#'
#' @references
#' ILO, IMF, OECD, UNECE, and World Bank. (2004).
#' *Producer Price Index Manual: Theory and Practice*.
#' International Monetary Fund.
#'
#' @examples
#' x <- 1:3
#' y <- 4:6
#' w <- 3:1

#' # Factor the harmonic mean by chaining the calculation.
#'
#' harmonic_mean(x * y, w)
#' harmonic_mean(x, w) * harmonic_mean(y, factor_weights(-1)(x, w))
#'
#' # The common case of an arithmetic mean.
#'
#' arithmetic_mean(x * y, w)
#' arithmetic_mean(x, w) * arithmetic_mean(y, update_weights(x, w))
#'
#' # In cases where x and y have the same order, Chebyshev's
#' # inequality implies that the chained calculation is too small.
#'
#' arithmetic_mean(x * y, w) >
#'   arithmetic_mean(x, w) * arithmetic_mean(y, w)
#'
#' @family weights functions
#' @export
factor_weights <- function(r) {
  r <- as.numeric(r)
  if (not_finite_scalar(r)) {
    stop("'r' must be a finite length 1 numeric")
  }

  function(x, w = NULL) {
    if (!is.null(w) && length(x) != length(w)) {
      stop("'x' and 'w' must be the same length")
    }
    if (r == 0) {
      if (is.null(w)) {
        w <- rep.int(1, length(x))
      }
      w[is.na(x)] <- NA_real_
      w
    } else {
      if (is.null(w)) {
        v <- x^r
        attributes(v) <- NULL
      } else {
        v <- w * x^r
        attributes(v) <- attributes(w)
      }
      v
    }
  }
}

#' Price update weights
#' @rdname factor_weights
#' @export
update_weights <- factor_weights(1)

#' Scale weights
#'
#' Scale a vector of weights so that they sum to 1.
#'
#' @inheritParams generalized_mean
#'
#' @returns
#' A numeric vector that sums to 1. If there are `NA`s in `x` then the result
#' sums 1 to if these values are removed.
#'
#' @seealso
#' [grouped()] to make this function applicable to grouped data.
#'
#' @examples
#' scale_weights(1:5)
#'
#' scale_weights(c(1:5, NA))
#'
#' @family weights functions
#' @export
scale_weights <- function(x) {
  x / sum(x, na.rm = TRUE)
}
