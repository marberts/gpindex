#---- Arithmetic mean ----
arithmetic_mean <- function (x, w, na.rm = FALSE, scale = TRUE) {
  stopifnot(
    "x must be numeric or logical" = is.numeric(x) || is.logical(x),
    "weights must be numeric or logical" = missing(w) || (is.numeric(w) || is.logical(w)), 
    "x and w must be the same length" = missing(w) || length(x) == length(w), 
    "na.rm must be a length 1 logical" = length(na.rm) == 1L && is.logical(na.rm),
    "scale must be a length 1 logical" = length(scale) == 1L && is.logical(scale)
  )
  if (missing(w)) {
    if (anyNA(x) && na.rm) {
      x <- x[!is.na(x)]
    }
    return(sum(x) * (scale / length(x) + 1 - scale))
  } else {
    if ((anyNA(x) || anyNA(w)) && na.rm) {
      na <- is.na(x) | is.na(w)
      x <- x[!na]
      w <- w[!na]
    }
    sum((x * w)[w != 0]) * (scale / sum(w) + 1 - scale)
  }
}

#---- Generalized mean ----
generalized_mean <- function (x, w, r, na.rm = FALSE, scale = TRUE) {
  # check input
  stopifnot(
    "r must be a length 1 numeric" = length(r) == 1L && is.numeric(r) && is.finite(r)
  )
  # geomean if r = 0
  if (r == 0) { 
    exp(arithmetic_mean(log(x), w, na.rm, scale))
  # r = +-1 cases are faster on their own without needless ^1
  } else if (abs(r) == 1) { 
    # arithmetic mean if r = 1
    if (r == 1) { 
      arithmetic_mean(x, w, na.rm, scale)
    # harmonic mean if r = -1
    } else { 
      1 / arithmetic_mean(1 / x, w, na.rm, scale)
    }
  # generalized mean otherwise
  # if r < 0 then 1 / x^r is faster than x^(-r)
  } else if (r < 0) { 
    1 / (arithmetic_mean((1 / x^abs(r)), w, na.rm, scale))^(1 / abs(r)) 
  } else {
    (arithmetic_mean(x^r, w, na.rm, scale))^(1 / r)
  }
}

#---- Geometric mean ----
geometric_mean <- function (x, w, na.rm = FALSE, scale = TRUE) generalized_mean(x, w, 0, na.rm, scale)

#---- Harmonic mean ----
harmonic_mean <- function (x, w, na.rm = FALSE, scale = TRUE) generalized_mean(x, w, -1, na.rm, scale)

#---- Generalized logarithmic mean ----
generalized_logmean <- function (a, b, r) {
  # check input
  stopifnot(
    "a must be numeric" = is.numeric(a), 
    "b must be numeric" = is.numeric(b),
    "r must be a length 1 numeric" = length(r) == 1L && is.numeric(r) && is.finite(r)
  )
  # return numeric(0) if either a or b is length 0
  if (length(a) == 0L || length(b) == 0L) return(numeric(0))
  # a and b must be the same length, so manually recycle if necessary
  if (length(a) > length(b)) {
    b <- rep_len(b, length(a))
  } else if (length(b) > length(a)) {
    a <- rep_len(a, length(b))
  }
  # calculate generalized logmean
  out <- if (r == 0) {
    # regular logmean if r = 0
    (a - b) / log(a / b)
  } else if (r == 1) {
    # r = 1
    (a^a / b^b)^(1 / (a - b)) / exp(1)
  } else {
    # general case
    if (r > 0 & r < 1) {
      (r * (a - b) / (a^r - b^r))^(1 / (1 - r))
    } else if (r < 0) {
      (r * (a - b) / (1 / a^abs(r) - 1 / b^abs(r)))^(1 / (1 - r))
    } else {
      ((a^r - b^r) / (r * (a - b)))^(1 / (r - 1))
    }
  }
  # set output to a when a = b
  loc <- which(abs(a - b) < .Machine$double.eps^0.5) 
  out[loc] <- a[loc]
  out
}

#---- Logarithmic mean ----
logmean <- function (a, b) generalized_logmean(a, b, 0)


