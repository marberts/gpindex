#---- Weights to turn an r-generalized mean into a k-generalized mean
weights_change <- function(r, s){
  stopifnot(
    "'r' must be a finite length 1 numeric vector" = 
      length(r) == 1L && is.vector(r, "numeric") && is.finite(r),
    "'s' must be a finite length 1 numeric vector" = 
      length(s) == 1L && is.vector(s, "numeric") && is.finite(s)
  )
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE, M) {
    # check input
    stopifnot(
      "'x' must be a numeric vector" = 
        is.vector(x, "numeric"),
      "'w' must be a numeric or logical vector" = 
        is.vector(w, "numeric") || is.vector(w, "logical"),
      "'x' and 'w' must be the same length" = 
        length(x) == length(w),
      "'na.rm' must be TRUE or FALSE" = 
        length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm),
      "'scale' must be TRUE or FALSE" = 
        length(scale) == 1L && is.logical(scale) && !is.na(scale),
      "'M' must be a length 1 numeric vector" = 
        missing(M) || (length(M) == 1L && is.vector(M, "numeric"))
    )
    # the whole thing might be faster using the extended mean in Bullen (2003, p. 393)
    out <- if (r == s) {
      as.numeric(w) # ensure result is numeric if input is logical
    } else {
      # calculate M if not provided
      if (missing(M)) M <- mean_generalized(r)(x, w, na.rm)
      w * logmean_generalized(r)(x, M) %^% (r - 1) / 
        logmean_generalized(s)(x, M) %^% (s - 1)
    }
    if (scale) weights_scale(out, na.rm) else out
  }
}

#---- Common cases ----
weights_g2a <- weights_change(0, 1)

weights_h2a <- weights_change(-1, 1)

weights_a2g <- weights_change(1, 0)

weights_h2g <- weights_change(-1, 0)

#---- Weights to factor a mean of products into the product of means ----
weights_factor <- function(r) {
  stopifnot(
    "'r' must be a finite length 1 numeric vector" = 
      length(r) == 1L && is.vector(r, "numeric") && is.finite(r)
  )
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
    # check inputs
    stopifnot(
      "'x' must be a numeric vector" = 
        is.vector(x, "numeric"),
      "'w' must be a numeric or logical vector" = 
        is.vector(w, "numeric") || is.vector(w, "logical"),
      "'x' and 'w' must be the same length" = 
        length(x) == length(w),
      "'na.rm' must be TRUE or FALSE" = 
        length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm),
      "'scale' must be TRUE or FALSE" = 
        length(scale) == 1L && is.logical(scale) && !is.na(scale)
    )
    if (r == 0) {
      # return w when r = 0
      out <- as.numeric(w)
      if (anyNA(x)) {
        # make sure NAs propegate
        out[is.nan(x) & !is.na(w)] <- NaN
        out[is.na(x) & !is.na(w)] <- NA
      }
    } else {
      # general case otherwise
      out <- w * x %^% r
    }
    if (scale) weights_scale(out, na.rm) else out
  }
}

#---- Common case ----
weights_update <- weights_factor(1)

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
