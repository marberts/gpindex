#---- Weights to turn an r-generalized mean into a k-generalized mean
weights_change <- function(x, w, r, k, na.rm = FALSE, scale = TRUE, M) {
  # check input
  check_mean_arguments(x, w, na.rm, scale) 
  stopifnot(
    "r must be a length 1 numeric" = length(r) == 1L && is.numeric(r) && is.finite(r),
    "k must be a length 1 numeric" = length(k) == 1L && is.numeric(k) && is.finite(k),
    "M must be a length 1 numeric" = missing(M) || (length(M) == 1L && is.numeric(M) && is.finite(M))
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
  # return w when r = k
  if (r == k) {
    out <- rep_len(w, length(x))
    #out[is.na(x)] <- NA # make sure NAs propegate
  } else {
    # calculate logmeans
    # r,k = 1 case does not need to be calculated because it's always 1
    lmr <- if (r != 1) logmean_generalized(x, M, r) else 1
    lmk <- if (k != 1) logmean_generalized(x, M, k) else 1
    # calculate exponent on logmeans
    # no exponents needed when r,k = 0 or 2
    plmr <- if (r == 2 || r == 0) lmr else lmr^abs(r - 1)
    plmk <- if (k == 2 || k == 0) lmk else lmk^abs(k - 1)
    # it's faster in key cases to do 1/x^y than x^(-y) (i.e., when y = 1 or 2)
    out <- if (r >= 1 && k >= 1) {
      w * plmr / plmk
    } else if (r < 1 && k >= 1) {
      w / (plmr * plmk)
    } else if (r >= 1 & k < 1) {
      w * plmr * plmk
    } else {
      w / plmr * plmk
    }
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
  check_mean_arguments(x, w, na.rm, scale) 
  stopifnot(
    "r must be a length 1 numeric" = length(r) == 1L && is.numeric(r) && is.finite(r)
  )
  # set w if equally weighted
  if (missing(w)) {
    w <- if (length(x)) 1 else numeric(0)
  }
  # return w when r = 0
  if (r == 0) {
    out <- rep_len(w, length(x))
    out[is.na(x)] <- NA # make sure NAs propegate
    # r = +-1 cases are faster on their own without needless ^1
  } else if (abs(r) == 1) {
    if (r == 1) {
      out <- w * x
    } else {
      out <- w / x
    }
    # general case otherwise
    # there are some ways to boost performance when r =-2, 0.5, but I don't think it's worth it
  } else {
    out <- w * x^r # the general equation
  }
  if (scale) weights_scale(out, na.rm) else out
}

#---- Common case ----
# index_price_update <- function(p1, p0, q1, q0, type, na.rm = FALSE, scale = TRUE) {
#   check_weights_arguments(p1, p0, q1, q0)
#   type <- match.arg(type, types$arithmetic_index_types)
#   w <- index_weights(p1, p0, q1, q0, type)
#   weights_factor(p1 / p0, w, 1, na.rm, scale)
# }

#---- Scale weights ----
weights_scale <- function(w, na.rm = FALSE) {
  stopifnot(
    "w must be numeric or logical" = is.numeric(w) || is.logical(w),
    "na.rm must be a length 1 logical" = length(na.rm) == 1L && is.logical(na.rm)
  )
  if (!na.rm && anyNA(w)) return(rep.int(NA_real_, length(w)))
  w / sum(w, na.rm = TRUE)
}
