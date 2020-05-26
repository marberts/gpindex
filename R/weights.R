# #---- Weights to turn an r-generalized mean into a k-generalized mean
# weights_change <- function (x, w, r, k, na.rm = FALSE, M) {
#   # check input
#   stopifnot(
#     "k must be length 1 numeric " = length(k) == 1L && is.numeric(k) && is.finite(k),
#     "M must be a length 1 numeric" = missing(M) || (length(M) == 1L && is.numeric(M))
#   )
#   # set w if equally weighted 
#   if (missing(w)) {
#     w <- if (length(x)) 1 else numeric(0)
#     # calculate r-mean with equal weights
#     if (missing(M)) {
#       M <- mean_generalized(x, r = r, na.rm = na.rm)
#     }
#   # calculate r-mean with unequal weights
#   } else if (missing(M)) {
#     M <- mean_generalized(x, w, r, na.rm = na.rm)
#   }
#   # return w when r = k
#   if (r == k) {
#     rep_len(w, length(x)) * !is.na(x) # make sure NAs propegate
#   # r, k = 2 cases are faster on their own without needless ^1
#   } else if (r == 2 || k == 2) {
#     if (r == 2) {
#       w * logmean_generalized(x, M, r) / logmean_generalized(x, M, k)^(k - 1)
#     } else {
#       w * logmean_generalized(x, M, r)^(r - 1) / logmean_generalized(x, M, k)
#     }
#   # r, k = 1 cases are faster on their own without needless ^0 
#   } else if (r == 1 || k == 1) {
#     if (r == 1) {
#       w / logmean_generalized(x, M, k)^(k - 1)
#     } else {
#       w * logmean_generalized(x, M, r)^(r - 1)
#     }
#   # r, k = 0 cases are faster on their own without needless ^-1
#   } else if (r == 0 || k == 0) {
#     if (r == 0) {
#       w / (logmean_generalized(x, M, r) * logmean_generalized(x, M, k)^(k - 1))
#     } else {
#       w * logmean_generalized(x, M, r)^(r - 1) * logmean_generalized(x, M, k)
#     }
#   # general case otherwise
#   } else {
#     w * logmean_generalized(x, M, r)^(r - 1) / logmean_generalized(x, M, k)^(k - 1) # the general equation
#   }
# }

#---- Weights to turn an r-generalized mean into a k-generalized mean
weights_change <- function (x, w, r, k, na.rm = FALSE, M) {
  # check input
  stopifnot(
    "x must be numeric or logical" = is.numeric(x) || is.logical(x),
    "weights must be numeric or logical" = missing(w) || (is.numeric(w) || is.logical(w)), 
    "x and w must be the same length" = missing(w) || length(x) == length(w), 
    "r must be length 1 numeric " = length(r) == 1L && is.numeric(r) && is.finite(r),
    "k must be length 1 numeric " = length(k) == 1L && is.numeric(k) && is.finite(k),
    "M must be a length 1 numeric" = missing(M) || (length(M) == 1L && is.numeric(M))
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
  if (r == k) return(rep_len(w, length(x)) * !is.na(x)) # make sure NAs propegate
  # calculate logmeans
  # r,k = 1 case does not need to be calculated because it's always 1
  lmr <- if (r != 1) logmean_generalized(x, M, r) else 1
  lmk <- if (k != 1) logmean_generalized(x, M, k) else 1
  # calculate exponent on logmeans
  # no exponents needed when r,k = 0 or 2
  plmr <- if (r == 2 || r == 0) lmr else lmr^abs(r - 1)
  plmk <- if (k == 2 || k == 0) lmk else lmk^abs(k - 1)
  # it's faster in key cases to do 1/x^y than x^(-y) (i.e., when y = 1 or 2)
  if (r >= 1 && k >= 1) {
    w * plmr / plmk
  } else if (r < 1 && k >= 1) {
    w / (plmr * plmk)
  } else if (r >= 1 & k < 1) {
    w * plmr * plmk
  } else {
    w / plmr * plmk
  }
}
  
#---- Common cases ----
weights_g2a <- function (x, w, na.rm = FALSE, M) weights_change(x, w, 0, 1, na.rm, M)

weights_h2a <- function (x, w, na.rm = FALSE, M) weights_change(x, w, -1, 1, na.rm, M)

weights_a2g<- function (x, w, na.rm = FALSE, M) weights_change(x, w, 1, 0, na.rm, M)

weights_h2g <- function (x, w, na.rm = FALSE, M) weights_change(x, w, -1, 0, na.rm, M)

#---- Weights to factor a mean of products into the product of means ----
weights_factor <- function (x, w, r) {
  # check inputs
  stopifnot(
    "x must be numeric or logical" = is.numeric(x) || is.logical(x),
    "weights must be numeric or logical" = missing(w) || (is.numeric(w) || is.logical(w)), 
    "x and w must be the same length" = missing(w) || length(x) == length(w), 
    "r must be a length 1 numeric" = length(r) == 1L && is.numeric(r) && is.finite(r)
  )
  # set w if equally weighted 
  if (missing(w)) {
    w <- if (length(x)) 1 else numeric(0)
  }
  # return w when r = 0
  if (r == 0) {
    rep_len(w, length(x)) * !is.na(x) # make sure NAs propegate
  # r = +-1 cases are faster on their own without needless ^1
  } else if (abs(r) == 1) {
    if (r == 1) w * x else w / x
    
  # # there are some ways to boost performance, but I don't think it's worth it 
  # } else if (abs(r) == 0.5) {
  #   if (r == 0.5) {
  #     w * sqrt(x)
  #   } else {
  #     w / sqrt(x)
  #   }
  # } else if (r == -2) {
  #   w / x^abs(r)
  
  # general case otherwise  
  } else {
    w * x^r # the general equation
  }
}

#---- Common case ----
index_price_update <- function (x, w) weights_factor(x, w, 1)