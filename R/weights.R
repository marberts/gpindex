#---- Weights to turn an r-generalized mean into a k-generalized mean
weights_change <- function(x, w, r, k, na.rm = FALSE, scale = TRUE, M) {
  # check input
  stopifnot(
    "'x' must be a numeric or logical vector" = 
      is.vector(x, "numeric") || is.vector(x, "logical"),
    "'w' must be a numeric or logical vector" = 
      missing(w) || (is.vector(w, "numeric") || is.vector(w, "logical")),
    "'x' and 'w' must be the same length" = 
      missing(w) || length(x) == length(w),
    "'r' must be a finite length 1 numeric vector" = 
      length(r) == 1L && is.vector(r, "numeric") && is.finite(r),
    "'k' must be a finite length 1 numeric vector" = 
      length(k) == 1L && is.vector(k, "numeric") && is.finite(k),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm),
    "'scale' must be TRUE or FALSE" = 
      length(scale) == 1L && is.logical(scale) && !is.na(scale),
    "'M' must be a length 1 numeric vector" = 
      missing(M) || (length(M) == 1L && is.vector(M, "numeric"))
  )
  # set w if equally weighted
  if (missing(w)) {
    w <- if (length(x)) 1 else numeric(0)
    # calculate r-mean with equal weights
    if (missing(M)) {
      M <- mean_generalized(x, r = r, na.rm = na.rm)
    }
    # calculate r-mean with unequal weights
  } else if (missing(M)) {
    M <- mean_generalized(x, w, r, na.rm = na.rm)
  }
  # the whole thing might be faster using the extended mean in Bullen (2003, p. 393)
  if (r == k) {
    out <- rep_len(w, length(x))
  } else {
    out <- w * logmean_generalized(x, M, r) %^% (r - 1) / 
      logmean_generalized(x, M, k) %^% (k - 1)
  }
  if (scale) weights_scale(out, na.rm) else out
}

#---- Common cases ----
weights_g2a <- function(x, w, na.rm = FALSE, scale = TRUE, M) {
  weights_change(x, w, 0, 1, na.rm, scale, M)
}

weights_h2a <- function(x, w, na.rm = FALSE, scale = TRUE, M) {
  weights_change(x, w, -1, 1, na.rm, scale, M)
}

weights_a2g <- function(x, w, na.rm = FALSE, scale = TRUE, M) {
  weights_change(x, w, 1, 0, na.rm, scale, M)
}

weights_h2g <- function(x, w, na.rm = FALSE, scale = TRUE, M) {
  weights_change(x, w, -1, 0, na.rm, scale, M)
}

#---- Weights to factor a mean of products into the product of means ----
weights_factor <- function(x, w, r, na.rm = FALSE, scale = TRUE) {
  # check inputs
  stopifnot(
    "'x' must be a numeric or logical vector" = 
      is.vector(x, "numeric") || is.vector(x, "logical"),
    "'w' must be a numeric or logical vector" = 
      missing(w) || (is.vector(w, "numeric") || is.vector(w, "logical")),
    "'x' and 'w' must be the same length" = 
      missing(w) || length(x) == length(w),
    "'r' must be a finite length 1 numeric vector" = 
      length(r) == 1L && is.vector(r, "numeric") && is.finite(r),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm),
    "'scale' must be TRUE or FALSE" = 
      length(scale) == 1L && is.logical(scale) && !is.na(scale)
  )
  # set w if equally weighted
  if (missing(w)) {
    w <- if (length(x)) 1 else numeric(0)
  }
  if (r == 0) {
    # return w when r = 0
    out <- rep_len(w, length(x))
    out[is.na(x)] <- NA # make sure NAs propegate
  } else {
    # general case otherwise
    out <- w * x %^% r
  }
  if (scale) weights_scale(out, na.rm) else out
}

#---- Common case ----
weights_update <- function(x, w, na.rm = FALSE, scale = TRUE) {
  weights_factor(x, w, 1, na.rm, scale)
}

#---- Scale weights ----
weights_scale <- function(w, na.rm = FALSE) {
  stopifnot(
    "'w' must be a numeric or logical vector" = 
      is.vector(w, "numeric") || is.vector(w, "logical"),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  if (!na.rm && anyNA(w)) return(rep.int(NA_real_, length(w)))
  w / sum(w, na.rm = TRUE)
}
