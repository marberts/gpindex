#---- Weights to turn an r-generalized mean into a k-generalized mean
change_weights <- function (x, w, r, k, na.rm = FALSE, M) {
  stopifnot(
    "k must be length 1 numeric " = length(k) == 1L && is.numeric(k) && is.finite(k),
    "M must be a length 1 numeric" = missing(M) || (length(M) == 1L && is.numeric(M))
  )
  # set w to 1 / n if equally weighted and calculate r-mean
  if (missing(w)) {
    if (length(x) > 0) {
      w <- 1 / length(x)
    } else {
      w <- numeric(0)
    }
    if (missing(M)) {
      M <- generalized_mean(x, r = r, na.rm = na.rm)
    }
  } else if (missing(M)) {
    M <- generalized_mean(x, w, r, na.rm = na.rm)
  }
  if (r < 1 && k < 1) {
    w * generalized_logmean(x, M, k)^(1 - k) / generalized_logmean(x, M, r)^(1 - r)
  } else if (r < 1 && k >= 1) {
    if (k == 1) {
      w / generalized_logmean(x, M, r)^(1 - r)
    } else {
      w / (generalized_logmean(x, M, r)^(1 - r) * generalized_logmean(x, M, k)^(k - 1))
    }
  } else if (r >= 1 && k < 1) {
    if (r == 1) {
      w * generalized_logmean(x, M, k)^(1 - k)
    } else {
      w * generalized_logmean(x, M, r)^(r - 1) * generalized_logmean(x, M, k)^(1 - k)
    }
  } else if (r >= 1 && k >= 1) {
    if (r == 1 && k == 1) {
      rep_len(w, length(x))
    } else if (r == 1) {
      w / generalized_logmean(x, M, k)^(k - 1)
    } else if (k == 1){
      w * generalized_logmean(x, M, r)^(r - 1)
    } else {
      w * generalized_logmean(x, M, r)^(r - 1) / generalized_logmean(x, M, k)^(k - 1)
    }
  } 
}
  
#---- Common cases ----
geometric_to_arithmetic <- function (x, w, na.rm = FALSE, M) change_weights(x, w, 0, 1, na.rm, M)

harmonic_to_arithmetic <- function (x, w, na.rm = FALSE, M) change_weights(x, w, -1, 1, na.rm, M)

arithmetic_to_geometric <- function (x, w, na.rm = FALSE, M) change_weights(x, w, 1, 0, na.rm, M)

harmonic_to_geometric <- function (x, w, na.rm = FALSE, M) change_weights(x, w, -1, 0, na.rm, M)

#---- Weights to factor a mean of products into the product of means ----
factor_weights <- function(x, w, r) {
  stopifnot(
    "x must be numeric or logical" = is.numeric(x) || is.logical(x),
    "weights must be numeric or logical" = missing(w) || (is.numeric(w) || is.logical(w)), 
    "x and w must be the same length" = missing(w) || length(x) == length(w), 
    "r must be a length 1 numeric" = length(r) == 1L && is.numeric(r) && is.finite(r)
  )
  if (missing(w)) {
    if (length(x) > 0) {
      w <- 1 / length(x)
    } else {
      w <- numeric(0)
    }
  }
  if (r == 0) {
    rep_len(w, length(x))
  } else if (abs(r) == 1) {
    if (r == 1) {
      w * x
    } else {
      w / x
    }
  } else if (r < 0) {
    w / x^abs(r)
  } else {
    w * x^r
  }
}

#---- Common case ----
price_update <- function(x, w) factor_weights(x, w, r = 1)