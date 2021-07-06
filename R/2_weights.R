#---- Transmute weights ----
transmute_weights <- function(r, s) {
  gen_mean <- generalized_mean(r)
  ext_mean <- extended_mean(r, s)
  # return function
  function(x, w = rep(1, length(x))) {
    res <- w * ext_mean(x, gen_mean(x, w, na.rm = TRUE)) %^% (r - s)
    # make sure NAs propagate so that weights scale correctly with NAs in x
    res[if (r == s) is.na(x) & !is.na(w)] <- NA
    res
  }
}

#---- Factor weights  ----
factor_weights <- function(r) {
  if (!is_number(r)) {
    stop("'r' must be a finite length 1 numeric")
  }
  # return function
  function(x, w = rep(1, length(x))) {
    res <- w * x %^% r
    # make sure NAs propagate so that chaining works correctly with NAs in x
    res[if (r == 0) is.na(x) & !is.na(w)] <- NA
    res
  }
}

update_weights <- factor_weights(1)

#---- Scale weights ----
scale_weights <- function(x) {
  x / sum(x, na.rm = TRUE)
}

#---- Contributions ----
contributions <- function(r) {
  arithmetic_weights <- transmute_weights(r, 1)
  function(x, w = rep(1, length(x))) {
    scale_weights(arithmetic_weights(x, w)) * (x - 1)
  }
}

arithmetic_contributions <- contributions(1)

geometric_contributions <- contributions(0)

harmonic_contributions <- contributions(-1)

#---- Nested contributions ----
nested_contributions <- function(r, s, t = c(1, 1)) {
  contrib <- contributions(r)
  if (length(s) != 2) {
    stop("'s' must be a pair of numeric values")
  }
  r_weights1 <- transmute_weights(s[1], r)
  r_weights2 <- transmute_weights(s[2], r)
  if (length(t) != 2 || !is.numeric(t)) {
    stop("'t' must be a pair of numeric values")
  }
  t <- as.numeric(t) # strip attributes
  # return function
  function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x))) {
    v1 <- scale_weights(r_weights1(x, w1))
    v2 <- scale_weights(r_weights2(x, w2))
    # the calculation is wrong if NAs in w1 or w2 propagate
    v1[is.na(v1) & !is.na(v2)] <- 0
    v2[is.na(v2) & !is.na(v1)] <- 0
    contrib(x, t[1] * v1 + t[2] * v2)
  }
}

nested_contributions2 <- function(r, s, t = c(1, 1)) {
  arithmetic_weights <- transmute_weights(r, 1)
  if (length(s) != 2) {
    stop("'s' must be a pair of numeric values")
  }
  contrib1 <- contributions(s[1])
  contrib2 <- contributions(s[2])
  mean1 <- generalized_mean(s[1])
  mean2 <- generalized_mean(s[2])
  if (length(t) != 2 || !is.numeric(t)) {
    stop("'t' must be a pair of numeric values")
  }
  t <- as.numeric(t) # strip attributes
  # return function
  function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x))) {
    m <- c(mean1(x, w1, na.rm = TRUE), mean2(x, w2, na.rm = TRUE))
    v <- scale_weights(arithmetic_weights(m, t))
    u1 <- contrib1(x, w1)
    u2 <- contrib2(x, w2)
    # the calculation is wrong if NAs in w1 or w2 propagate
    u1[is.na(u1) & !is.na(u2)] <- 0
    u2[is.na(u2) & !is.na(u1)] <- 0
    v[1] * u1  + v[2] * u2 
  }
}

fisher_contributions <- nested_contributions(0, c(1, -1))

fisher_contributions2 <- nested_contributions2(0, c(1, -1))