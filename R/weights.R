#---- Weights to turn an r-generalized mean into a k-generalized mean
weights_change <- function(r, s){
  stopifnot("'r' must be a finite length 1 numeric vector" = 
              length1(r, "numeric") && is.finite(r),
            "'s' must be a finite length 1 numeric vector" = 
              length1(s, "numeric") && is.finite(s))
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
    stopifnot("'scale' must be TRUE or FALSE" = length1(scale, "logical"))
    # the whole thing might be faster using the extended mean in Bullen (2003, p. 393)
    M <- mean_generalized(r)(x, w, na.rm)
    res <- w * logmean_generalized(r)(x, M) %^% (r - 1) / 
      logmean_generalized(s)(x, M) %^% (s - 1)
    if (scale) weights_scale(res, na.rm) else res
  }
}

#---- Common cases ----
weights_g2a <- weights_change(0, 1)

weights_h2a <- weights_change(-1, 1)

weights_a2g <- weights_change(1, 0)

weights_h2g <- weights_change(-1, 0)

#---- Weights to factor a mean of products into the product of means ----
weights_factor <- function(r) {
  stopifnot("'r' must be a finite length 1 numeric vector" = 
              length1(r, "numeric") && is.finite(r))
  # return function
  function(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE) {
    stopifnot("'x' and 'w' must be a numeric vectors" = is_numeric(x, w),
              "'x' and 'w' must be the same length" = same_length(x, w),
              "'scale' must be TRUE or FALSE" = length1(scale, "logical"))
    res <- w * x %^% r
    # make sure NAs propegate
    if (r == 0) res[is.na(x) & !is.na(w)] <- NA
    if (scale) weights_scale(res, na.rm) else res
  }
}

#---- Common case ----
weights_update <- weights_factor(1)

#---- Scale weights ----
weights_scale <- function(w, na.rm = FALSE) {
  stopifnot("'w' must be a numeric vector" = is_numeric(w),
            "'na.rm' must be TRUE or FALSE" = length1(na.rm, "logical"))
  if (!na.rm && anyNA(w)) return(rep(NA_real_, length(w)))
  w / sum(w, na.rm = TRUE)
}
