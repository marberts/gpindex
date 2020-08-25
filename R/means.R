#---- Custom pow ----
# There are a variety of optimizations for calculating power means
`%^%` <- function(e1, e2) {
  e1 <- substitute(e1)
  res <- if (e2 == 1) {
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
  eval(res, parent.frame())
}

#---- Arithmetic mean ----
mean_arithmetic_ <- function(x, w, na.rm, scale) {
  # always return NA if there are any NAs in x or w (differs from weighted.mean)
  if (!na.rm && (anyNA(x) || anyNA(w))) return(NA_real_)
  d <- if (scale) sum(w[!is.na(x)], na.rm = TRUE) else 1
  sum(x * w, na.rm = TRUE) / d
}

#---- Generalized mean ----
mean_generalized <- function(r) {
  stopifnot("'r' must be a finite length 1 numeric vector" = 
              length1(r, "numeric") && is.finite(r))
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
    stopifnot("'x' and 'w' must be a numeric vectors" = is_numeric(x, w),
              "'x' and 'w' must be the same length" = same_length(x, w),
              "'na.rm' must be TRUE or FALSE" = length1(na.rm, "logical"),
              "'scale' must be TRUE or FALSE" = length1(na.rm, "logical"))
    if (abs(r) < .Machine$double.eps^0.5) {
      # geomean if r = 0 (can't do exact test or limits don't work well)
      exp(mean_arithmetic_(log(x), w, na.rm, scale))
    } else {
      # the general equation otherwise
      mean_arithmetic_(x %^% r, w, na.rm, scale) %^% (1 / r) 
    }
  }
}

#--- Arithmetic mean (exported) ---
mean_arithmetic <- mean_generalized(1)

#---- Geometric mean ----
mean_geometric <- mean_generalized(0)

#---- Harmonic mean ----
mean_harmonic <- mean_generalized(-1)

#---- Generalized logarithmic mean ----
logmean_generalized <- function(r) {
  stopifnot("'r' must be a finite length 1 numeric vector" = 
              length1(r, "numeric") && is.finite(r))
  # return function
  function(a, b, tol = .Machine$double.eps^0.5) {
    stopifnot("'a' and 'b' must be numeric vectors" = is_numeric(a, b),
              "'tol' must be a non-negative length 1 numeric vector" = 
                length1(tol, "numeric") && tol >= 0)
    # a and b must be the same length, so recycle if necessary
    if (length(a) > length(b) && length(b)) {
      if (length(a) %% length(b)) {
        warning("length of 'a' is not a multiple of length of 'b'")
      }
      b <- rep_len(b, length(a))
    } else if (length(b) > length(a) && length(a)) {
      if (length(b) %% length(a)) {
        warning("length of 'b' is not a multiple of length of 'a'")
      }
      a <- rep_len(a, length(b))
    }
    res <- if (abs(r) < .Machine$double.eps^0.5) {
      # regular logmean if r = 0
      (a - b) / log(a / b)
    } else if (abs(r - 1) < .Machine$double.eps^0.5) {
      (a^a / b^b)^(1 / (a - b)) / exp(1)
    } else {
      # general case otherwise
      ((a %^% r - b %^% r) / (a - b) / r) %^% (1 / (r - 1))
    }
    # set output to a when a = b
    loc <- which(abs(a - b) <= tol)
    res[loc] <- a[loc]
    res
  }
}

#---- Logarithmic mean ----
logmean <- logmean_generalized(0)