#---- Helper functions ----
# Argument checking
is_T_or_F <- function(x) {
  length(x) == 1 && is.logical(x) && !is.na(x)
}

is_number <- function(x) {
  length(x) == 1 && is.numeric(x) && is.finite(x)
}

is_positive_number <- function(x) {
  is_number(x) && x >= 0
}

all_same_length <- function(...) {
  res <- vapply(list(...), length, numeric(1))
  all(res == res[1])
}

all_numeric <- function(...) {
  all(vapply(list(...), is.numeric, logical(1)))
}

# Custom pow
# There are a variety of optimizations for calculating power/extended means
`%^%` <- function(e1, e2) {
  e1 <- substitute(e1)
  res <- if (e2 == 1) {
    e1
  } else if (e2 == 0.5) {
    call("sqrt", e1)
  } else if (e2 == 0) {
    1
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

#---- Arithmetic mean (internal) ----
mean_arithmetic_ <- function(x, w, na.rm, scale) {
  # return NA if there are NAs in x or w; differs from stats::weighted.mean
  if (!na.rm && (anyNA(x) || anyNA(w))) return(NA_real_)
  total <- sum(x * w, na.rm = TRUE)
  if (!scale) return(total)
  total / sum(if (na.rm) w[!is.na(x)] else w, na.rm = TRUE)
}

#---- Generalized mean ----
mean_generalized <- function(r) {
  stopifnot("'r' must be a finite length 1 numeric" = is_number(r))
  if (abs(r) < .Machine$double.eps^0.5 && r != 0) {
    warning("'r' is very small in absolute value, but not zero; this can give misleading results")
  }
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
    stopifnot("'x' and 'w' must be numeric vectors" = all_numeric(x, w),
              "'x' and 'w' must be the same length" = all_same_length(x, w),
              "'na.rm' must be TRUE or FALSE" = is_T_or_F(na.rm),
              "'scale' must be TRUE or FALSE" = is_T_or_F(scale))
    if (any(x < 0 | w < 0, na.rm = TRUE)) {
      warning("Some elements of 'x' or 'w' are negative; the generalized mean is not defined")
    }
    # this works more-or-less the same as genmean in StatsBase.jl
    if (r == 0) {
      exp(mean_arithmetic_(log(x), w, na.rm, scale))
    } else {
      mean_arithmetic_(x %^% r, w, na.rm, scale)^(1 / r) 
    }
  }
}

#---- Arithmetic mean (exported) ----
mean_arithmetic <- mean_generalized(1)

#---- Geometric mean ----
mean_geometric <- mean_generalized(0)

#---- Harmonic mean ----
mean_harmonic <- mean_generalized(-1)

#---- Extended mean ----
mean_extended <- function(r, s) {
  stopifnot("'r' must be a finite length 1 numeric" = is_number(r),
            "'s' must be a finite length 1 numeric" = is_number(s))
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
    stopifnot("'a' and 'b' must be numeric vectors" = all_numeric(a, b),
              "'tol' must be a non-negative length 1 numeric" = is_positive_number(tol))
    if (any(a <= 0 | b <= 0, na.rm = TRUE)) {
      warning("Some elements 'a' or 'b' are non-positive; the extended mean is not defined")
    }
    res <- if (r == 0 && s == 0) {
      sqrt(a * b)
    } else if (r == 0) {
      ((a %^% s - b %^% s) / (log(a) - log(b)) / s) %^% (1 / s)
    } else if (s == 0) {
      ((a %^% r - b %^% r) / (log(a) - log(b)) / r) %^% (1 / r)
    } else if (r == s) {
      exp(((a %^% r) * log(a) - (b %^% r) * log(b)) / (a %^% r - b %^% r) - 1 / r)
    } else {
      ((a %^% s - b %^% s) / (a %^% r - b %^% r) * r / s) %^% (1 / (s - r))
    }
    # set output to a when a == b
    loc <- which(abs(a - b) <= tol)
    res[loc] <- a[(loc - 1) %% length(a) + 1] # wrap-around indexing
    res
  }
}

#---- Generalized logarithmic mean ----
logmean_generalized <- function(r) mean_extended(r, 1)

#---- Logarithmic mean ----
logmean <- logmean_generalized(0)