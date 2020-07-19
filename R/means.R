#---- Custom pow ----
# There are a variety of optimizations for calculating power means
`%^%` <- function(e1, e2) {
  e1 <- substitute(e1)
  out <- if (e2 == 1) {
    e1
  } else if (e2 == 0.5) {
    call("sqrt", e1)
  } else if (e2 == 0) {
    call("(", 1)
  } else if (e2 == -0.5) {
    call("/", 1, call("sqrt", e1))
  } else if (e2 == -1) {
    call("/", 1, e1)
  } else if (e2 == -2) {
    call("/", 1, call("^", e1, 2))
  } else {
    call("^", e1, e2)
  }
  eval(out, parent.frame())
}

#---- Arithmetic mean ----
mean_arithmetic_ <- function(x, w, na.rm, scale) {
  if (!na.rm) {
    # return NA if there are any NAs in x or w (differs from weighted.mean)
    if (anyNA(x) || anyNA(w)) return(NA_real_)
    # otherwise the denominator is 1 / sum(w)
    d <- if (scale) sum(w) else 1  
  } else {
    # this seems to be faster than x <- x[!is.na(x) && !is.na(w)] (same for w)
    d <- if (scale) sum(w[!is.na(x)], na.rm = TRUE) else 1
  }
  sum(x * w, na.rm = TRUE) / d
}

#---- Generalized mean ----
mean_generalized <- function(x, w = rep(1, length(x)), r, na.rm = FALSE, scale = TRUE) {
  # check input
  # i've thought about making a function to check inputs, but this is more explicit
  # it also produces nicer error messages
  stopifnot(
    "'x' must be a numeric or logical vector" = 
      is.vector(x, "numeric") || is.vector(x, "logical"),
    "'w' must be a numeric or logical vector" = 
      is.vector(w, "numeric") || is.vector(w, "logical"),
    "'x' and 'w' must be the same length" = 
      length(x) == length(w),
    "'r' must be a length 1 numeric vector" = 
      length(r) == 1L && is.vector(r, "numeric") && !is.na(r),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm),
    "'scale' must be TRUE or FALSE" = 
      length(scale) == 1L && is.logical(scale) && !is.na(scale)
  )
  if (abs(r) < .Machine$double.eps^0.5) {
    # geomean if r = 0 (can't do exact test or limits don't work well)
    exp(mean_arithmetic_(log(x), w, na.rm, scale))
  } else if (is.finite(r)) {
    # the general equation otherwise
    mean_arithmetic_(x %^% r, w, na.rm, scale) %^% (1 / r) 
  } else if (r == Inf) {
    # +inf returns max
    max(x, na.rm = na.rm)
  } else {
    # -inf returns min
    min(x, na.rm = na.rm)
  }
}

#--- Arithmetic mean (exported) ---
mean_arithmetic <- function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
  mean_generalized(x, w, 1, na.rm, scale)
}

#---- Geometric mean ----
mean_geometric <- function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
  mean_generalized(x, w, 0, na.rm, scale)
}

#---- Harmonic mean ----
mean_harmonic <- function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
  mean_generalized(x, w, -1, na.rm, scale)
}

#---- Generalized logarithmic mean ----
logmean_generalized <- function(a, b, r, tol = .Machine$double.eps^0.5) {
  # check input
  stopifnot(
    "'a' must be a numeric vector" =
      is.vector(a, "numeric"),
    "'b' must be a numeric vector" =
      is.vector(b, "numeric"),
    "'r' must be a length 1 numeric vector" =
      length(r) == 1L && is.vector(r, "numeric") && !is.na(r),
    "'tol' must be a non-negative length 1 numeric vector" =
      length(tol) == 1L && is.vector(tol, "numeric") && is.finite(tol) && tol >= 0
  )
  # return numeric(0) if either a or b is length 0
  if (!length(a) || !length(b)) return(numeric(0))
  # a and b must be the same length, so recycle if necessary
  if (length(a) > length(b)) {
    if (length(a) %% length(b)) {
      warning("length of 'a' is not a multiple of length of 'b'")
    }
    b <- rep_len(b, length(a))
  } else if (length(b) > length(a)) {
    if (length(b) %% length(a)) {
      warning("length of 'b' is not a multiple of length of 'a'")
    }
    a <- rep_len(a, length(b))
  }
  # calculate generalized logmean
  out <- if (abs(r) < .Machine$double.eps^0.5) {
    # regular logmean if r = 0
    (a - b) / log(a / b)
  } else if (abs(r - 1) < .Machine$double.eps^0.5) {
    (a^a / b^b)^(1 / (a - b)) / exp(1)
  } else if (is.finite(r)) {
    # general case otherwise
    ((a %^% r - b %^% r) / (a - b) / r) %^% (1 / (r - 1))
  } else if (r == Inf) {
    # +inf returns max
    pmax(a, b)
  } else {
    # -inf returns min
    pmin(a, b)
  }
  # set output to a when a = b
  loc <- which(abs(a - b) <= tol)
  out[loc] <- a[loc]
  out
}

#---- Logarithmic mean ----
logmean <- function(a, b, tol = .Machine$double.eps^0.5) {
  logmean_generalized(a, b, 0, tol)
}