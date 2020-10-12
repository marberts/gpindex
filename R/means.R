#---- Arithmetic mean (internal) ----
mean_arithmetic_ <- function(x, w, na.rm, scale) {
  # return NA if there are NAs in x or w; differs from stats::weighted.mean
  if (!na.rm && (anyNA(x) || anyNA(w))) return(NA_real_)
  tot <- sum(x * w, na.rm = TRUE)
  if (!scale) return(tot)
  tot / sum(if (na.rm) w[!is.na(x)] else w, na.rm = TRUE)
}

#---- Generalized mean ----
mean_generalized <- function(r) {
  stopifnot("'r' must be a finite length 1 numeric vector" = length1(r, "numeric"))
  if (abs(r) < .Machine$double.eps^0.5 && r != 0) {
    warning("'r' is very small in absolute value, but not zero; this can give misleading results")
  }
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
    stopifnot("'x' and 'w' must be numeric vectors" = is_numeric(x, w),
              "'x' and 'w' must be the same length" = same_length(x, w),
              "'na.rm' must be TRUE or FALSE" = length1(na.rm, "logical"),
              "'scale' must be TRUE or FALSE" = length1(na.rm, "logical"))
    if (any(x < 0 | w < 0, na.rm = TRUE)) {
      warning("Some elements of 'x' or 'w' are negative")
    }
    # this works more-or-less the same as genmean in StatsBase.jl
    if (r == 0) {
      exp(mean_arithmetic_(log(x), w, na.rm, scale))
    } else {
      mean_arithmetic_(x %^% r, w, na.rm, scale)^(1 / r) 
    }
  }
}

#--- Arithmetic mean (exported) ---
mean_arithmetic <- mean_generalized(1)

#---- Geometric mean ----
mean_geometric <- mean_generalized(0)

#---- Harmonic mean ----
mean_harmonic <- mean_generalized(-1)

#---- Extended mean (internal) ----
mean_extended_ <- function(r, s, transmute = FALSE) {
  stopifnot("'r' must be a finite length 1 numeric vector" = length1(r, "numeric"),
            "'s' must be a finite length 1 numeric vector" = length1(s, "numeric"))
  if (abs(r) < .Machine$double.eps^0.5 && r != 0) {
    warning("'r' is very small in absolute value, but not zero; this can give misleading results")
  }
  if (abs(s) < .Machine$double.eps^0.5 && s != 0) {
    warning("'s' is very small in absolute value, but not zero; this can give misleading results")
  }
  if (abs(r - s) < .Machine$double.eps^0.5 && r != s) {
    warning("'r' and 's' are very close in value, but not equal; this can give misleading results")
  }
  # return function
  function(a, b, tol = .Machine$double.eps^0.5) {
    stopifnot("'a' and 'b' must be numeric vectors" = is_numeric(a, b),
              "'tol' must be a non-negative length 1 numeric vector" = 
                length1(tol, "numeric") && tol >= 0)
    if (any(a <= 0 | b <= 0, na.rm = TRUE)) {
      warning("Some elements 'a' or 'b' are non-positive")
    }
    if (r == 0 && s == 0) {
      e <- if (transmute) 0 else 1
      res <- sqrt(a * b)
    } else if (r == 0) {
      e <- if (transmute) -1 else 1 / s
      res <- (a %^% s - b %^% s) / (log(a) - log(b)) / s
    } else if (s == 0) {
      e <- if (transmute) 1 else 1 / r
      res <- (a %^% r - b %^% r) / (log(a) - log(b)) / r
    } else if (r == s) {
      e <- if (transmute) 0 else 1
      res <- exp(((a %^% r) * log(a) - (b %^% r) * log(b)) / 
                   (a %^% r - b %^% r) - 1 / r)
    } else {
      e <- if (transmute) 1 else 1 / (r - s)
      res <- (a %^% r - b %^% r) / (a %^% s - b %^% s) * s / r
    }
    res <- res %^% e
    # set output to a when a = b
    loc <- which(abs(a - b) <= tol)
    res[loc] <- a[(loc - 1L) %% length(a) + 1] # wrap-around indexing
    res
  }
}

#---- Extended mean (exported) ----
mean_extended <- function(r, s) mean_extended_(r, s)

#---- Generalized logarithmic mean ----
logmean_generalized <- function(r) mean_extended(r, 1)

#---- Logarithmic mean ----
logmean <- logmean_generalized(0)