#---- Internal mean ----
# similar to stats::weighted.mean, except that 0s in w are not
# strong 0s, and na.rm = TRUE removes NAs in x and w

.mean <- function(x, w, na.rm) {
  if (na.rm) {
    if (anyNA(x) || anyNA(w)) {
      keep <- !(is.na(x) | is.na(w))
      x <- x[keep]
      w <- w[keep]
    } 
  }
  sum(w * x) / sum(w)
}

#---- Generalized mean ----
generalized_mean <- function(r) {
  if (!is_number(r)) {
    stop("'r' must be a finite length 1 numeric")
  }
  if (small_but_not_zero(r)) {
    warning("'r' is very small in absolute value, but not zero; this can give misleading results")
  }
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE) {
    if (length(x) != length(w)) {
      stop("'x' and 'w' must be the same length")
    }
    if (any_negative(x, w)) {
      warning("some elements of 'x' or 'w' are less than or equal to 0; the generalized mean is not defined")
    }
    # this works more-or-less the same as genmean in StatsBase.jl
    if (r == 0) {
      exp(.mean(log(x), w, na.rm))
    } else {
      .mean(x %^% r, w, na.rm)^(1 / r) 
    }
  }
}

#---- Pythagorean means ----
arithmetic_mean <- generalized_mean(1)

geometric_mean <- generalized_mean(0)

harmonic_mean <- generalized_mean(-1)

#---- Extended mean ----
extended_mean <- function(r, s) {
  if (!is_number(r) || !is_number(s)) {
    stop("'r' and 's' must be finite length 1 numerics")
  }
  if (small_but_not_zero(r)) {
    warning("'r' is very small in absolute value, but not zero; this can give misleading results")
  }
  if (small_but_not_zero(s)) {
    warning("'s' is very small in absolute value, but not zero; this can give misleading results")
  }
  if (small_but_not_zero(r - s)) {
    warning("'r' and 's' are very close in value, but not equal; this can give misleading results")
  }
  # return function
  function(a, b, tol = .Machine$double.eps^0.5) {
    if (any_negative(a, b)) {
      warning("some elements of 'a' or 'b' are less than or equal to 0; the extended mean is not defined")
    }
    res <- if (r == 0 && s == 0) {
      sqrt(a * b)
    } else if (r == 0) {
      ((a %^% s - b %^% s) / log(a / b) / s) %^% (1 / s)
    } else if (s == 0) {
      ((a %^% r - b %^% r) / log(a / b) / r) %^% (1 / r)
    } else if (r == s) {
      exp(((a %^% r) * log(a) - (b %^% r) * log(b)) / (a %^% r - b %^% r) - 1 / r)
    } else {
      ((a %^% s - b %^% s) / (a %^% r - b %^% r) * r / s) %^% (1 / (s - r))
    }
    # set output to a when a == b
    loc <- which(abs(a - b) <= tol)
    res[loc] <- a[wrap_around(a, loc)]
    res
  }
}

#---- Logarithmic means ----
generalized_logmean <- function(r) {
  extended_mean(r, 1)
}

logmean <- generalized_logmean(0)

#---- Lehmer mean ----
lehmer_mean <- function(r) {
  if (!is_number(r)) {
    stop("'r' must be a finite length 1 numeric")
  }
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE) {
    arithmetic_mean(x, w * x %^% (r - 1), na.rm)
  }
}

contraharmonic_mean <- lehmer_mean(2)

#---- Nested mean ----
nested_mean <- function(r, s, t = c(1, 1)) {
  outer_mean <- generalized_mean(r)
  if (length(s) != 2) {
    stop("'s' must be a pair of numeric values")}
  inner_mean1 <- generalized_mean(s[1])
  inner_mean2 <- generalized_mean(s[2])
  if (length(t) != 2 || !is.numeric(t)) {
    stop("'t' must be a pair of numeric values")
  }
  t <- as.numeric(t) # strip any attributes
  # return function
  function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x)), na.rm = FALSE) {
    x <- c(inner_mean1(x, w1, na.rm), inner_mean2(x, w2, na.rm))
    outer_mean(x, t, na.rm)
  }
}

fisher_mean <- nested_mean(0, c(1, -1))