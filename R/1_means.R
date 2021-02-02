#---- Internal mean ----
# similar to stats::weighted.mean, except that 0s in w are not
# strong 0s, and na.rm = TRUE removes NAs in x and w

.mean <- function(x, w, na.rm, scale) {
  if (na.rm) {
    if (anyNA(x) || anyNA(w)) {
      keep <- !(is.na(x) | is.na(w))
      x <- x[keep]
      w <- w[keep]
    } 
  }
  sum(w * x) / if (scale) sum(w) else 1
}

#---- Generalized mean ----
mean_generalized <- function(r) {
  if (!is_number(r)) {
    stop("'r' must be a finite length 1 numeric")
  }
  if (small_but_not_zero(r)) {
    warning("'r' is very small in absolute value, but not zero; this can give misleading results")
  }
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
    if (length(x) != length(w)) {
      stop("'x' and 'w' must be the same length")
    }
    if (any_negative(x, w)) {
      warning("Some elements of 'x' or 'w' are less than or equal to 0; the generalized mean is not defined")
    }
    # this works more-or-less the same as genmean in StatsBase.jl
    if (r == 0) {
      exp(.mean(log(x), w, na.rm, scale))
    } else {
      .mean(x %^% r, w, na.rm, scale)^(1 / r) 
    }
  }
}

#---- Pythagorean means ----
mean_arithmetic <- mean_generalized(1)

mean_geometric <- mean_generalized(0)

mean_harmonic <- mean_generalized(-1)

#---- Extended mean ----
mean_extended <- function(r, s) {
  if (!is_number(r) || !is_number(s)) {
    stop("'r' and 's' must be a finite length 1 numerics")
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
      warning("Some elements 'a' or 'b' are less than or equal to 0; the extended mean is not defined")
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
    replace(res, loc, a[wrap_around(a, loc)])
  }
}

#---- Logarithmic means ----
logmean_generalized <- function(r) {
  mean_extended(r, 1)
}

logmean <- logmean_generalized(0)

#---- Lehmer mean ----
mean_lehmer <- function(r) {
  if (!is_number(r)) {
    stop("'r' must be a finite length 1 numeric")
  }
  function(x, w = rep(1, length(x)), na.rm = FALSE) {
    mean_arithmetic(x, w * x %^% (r - 1), na.rm, scale = TRUE)
  }
}

mean_contraharmonic <- mean_lehmer(2)