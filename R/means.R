#---- Arithmetic mean ----
mean_arithmetic <- function (x, w, na.rm = FALSE, scale = TRUE) {
  # check input
  stopifnot(
    "x must be numeric or logical" = is.numeric(x) || is.logical(x),
    "weights must be numeric or logical" = missing(w) || (is.numeric(w) || is.logical(w)), 
    "x and w must be the same length" = missing(w) || length(x) == length(w), 
    "na.rm must be a length 1 logical" = length(na.rm) == 1L && is.logical(na.rm),
    "scale must be a length 1 logical" = length(scale) == 1L && is.logical(scale)
  )
  # unweighted case
  if (missing(w)) {
    # return NA if there are any NAs in x
    # this means that NaN returns NA, not NaN as with stats::weighted.mean
    if (!na.rm && anyNA(x)) {
      return(NA_real_)
    } else {
      # anyNA(x) gets called twice when na.rm = FALSE, 
      # but this seems to be faster than x <- x[!is.na(x)]
      denom <- if (anyNA(x)) sum(!is.na(x), na.rm = TRUE) else length(x)
      return(sum(x, na.rm = TRUE) * (scale / denom  + 1 - scale))
    }
  # weighted case
  } else {
    # return NA if there are any NAs in x or w
    # again differents from weighted.mean when there are NaNs
    if (!na.rm && (anyNA(x) || anyNA(w))) {
      return(NA_real_)
    } else {
      # anyNA(x) gets called twice when na.rm = FALSE, 
      # but this seems to be faster than x <- x[!is.na(x) && !is.na(w)]
      denom <- if (anyNA(x)) sum(w[!is.na(x)], na.rm = TRUE) else sum(w, na.rm = TRUE)
      sum(x * w, na.rm = na.rm) * (scale / denom + 1 - scale)
    }
  }
}

# Exponents of -1, 0, 1, and 2 are the most important, so there are some optimizations for these cases.

#---- Generalized mean ----
mean_generalized <- function (x, w, r, na.rm = FALSE, scale = TRUE) {
  # check input
  stopifnot(
    "r must be a length 1 numeric" = length(r) == 1L && is.numeric(r) && is.finite(r)
  )
  # geomean if r = 0
  if (r == 0) { 
    exp(mean_arithmetic(log(x), w, na.rm, scale))
  # r = +-1 cases are faster on their own without needless ^1
  } else if (abs(r) == 1) { 
    # arithmetic mean if r = 1
    if (r == 1) { 
      mean_arithmetic(x, w, na.rm, scale)
    # harmonic mean if r = -1
    } else { 
      1 / mean_arithmetic(1 / x, w, na.rm, scale)
    }
    
  # # there are some ways to boost performance, but I don't think it's worth the complexity  
  # # if r = 0.5 then sqrt(x) is faster than x^0.5
  # } else if (abs(r) == 0.5) {
  #   if (r == 0.5) {
  #     mean_arithmetic(sqrt(x), w, na.rm, scale)^(1 / r)
  #   } else {
  #     1 / (mean_arithmetic(1 / sqrt(x), w, na.rm, scale))^(1 / abs(r))
  #   }
  # # if r == -2 then 1 / x^2 is faster than x^(-2)
  # } else if (r == -2) { 
  #   1 / (mean_arithmetic((1 / x^abs(r)), w, na.rm, scale))^(1 / abs(r)) 
  # }  
    
  # generalized mean otherwise
  } else {
    (mean_arithmetic(x^r, w, na.rm, scale))^(1 / r) # the general equation
  }
}

#---- Geometric mean ----
mean_geometric <- function (x, w, na.rm = FALSE, scale = TRUE) mean_generalized(x, w, 0, na.rm, scale)

#---- Harmonic mean ----
mean_harmonic <- function (x, w, na.rm = FALSE, scale = TRUE) mean_generalized(x, w, -1, na.rm, scale)

#---- Generalized logarithmic mean ----
logmean_generalized <- function (a, b, r, tol = .Machine$double.eps^0.5) {
  # check input
  stopifnot(
    "a must be numeric" = is.numeric(a), 
    "b must be numeric" = is.numeric(b),
    "r must be a length 1 numeric" = length(r) == 1L && is.numeric(r) && is.finite(r),
    "tol must be a length 1 numeric" = length(tol) == 1L && is.numeric(tol) && is.finite(tol)
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
  # regular logmean if r = 0
  out <- if (r == 0) {
    (a - b) / log(a / b)
  } else if (abs(r) == 1) {
    if (r == 1) {
      (a^a / b^b)^(1 / (a - b)) / exp(1)
      # 1/x is faster than x^(-1) and sqrt is faster than x^0.5
    } else {
      sqrt((r * (a - b) / (1 / a - 1 / b)))
    }
  } else if (r == 2) {
    # r = 2 case is faster without needless ^1
    (a^r - b^r) / (r * (a - b))
  # general case otherwise  
  } else {
    
    # there are a number of ways to boost performance is some cases, but I don't think it's worth it
    # if (abs(r) == 2) {
    #   if (r == 2) {
    #     (a^r - b^r) / (r * (a - b))
    #   } else {
    #     (r * (a - b) / (1 / a^abs(r) - 1 / b^abs(r)))^(1 / (1 - r))
    #   }
    # } else if (abs(r) == 0.5) {
    #   if (r == 0.5) {
    #     (r * (a - b) / (sqrt(a) - sqrt(b)))^(1 / (1 - r))
    #   } else {
    #     ((1 / sqrt(a) - 1 / sqrt(b)) / (r * (a - b)))^(1 / (r - 1))
    #   }
    # } else if (r == 3) {
    #   sqrt(((a^r - b^r) / (r * (a - b))))
    # }
    
    ((a^r - b^r) / (r * (a - b)))^(1 / (r - 1)) # the general equation
    # this is marginally slower than a cpp implementation
  }
  # set output to a when a = b
  loc <- which(abs(a - b) <= tol) 
  out[loc] <- a[loc]
  out
}

#---- Logarithmic mean ----
logmean <- function (a, b, tol = .Machine$double.eps^0.5) logmean_generalized(a, b, 0, tol)


