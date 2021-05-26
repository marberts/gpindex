#---- Transmute weights ----
weights_transmute <- function(r, s) {
  generalized_mean <- mean_generalized(r)
  extended_mean <- mean_extended(r, s)
  # return function
  function(x, w = rep(1, length(x))) {
    res <- w * extended_mean(x, generalized_mean(x, w, na.rm = TRUE)) %^% (r - s)
    # make sure NAs propagate so that weights scale correctly with NAs in x
    res[if (r == s) is.na(x) & !is.na(w)] <- NA
    res
  }
}

#---- Factor weights  ----
weights_factor <- function(r) {
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

weights_update <- weights_factor(1)

#---- Scale weights ----
weights_scale <- function(x) {
  x / sum(x, na.rm = TRUE)
}

#---- Contributions ----
contributions <- function(r) {
  arithmetic_weights <- weights_transmute(r, 1)
  function(x, w = rep(1, length(x))) {
    weights_scale(arithmetic_weights(x, w)) * (x - 1)
  }
}

contributions_arithmetic <- contributions(1)

contributions_geometric <- contributions(0)

contributions_harmonic <- contributions(-1)

contributions_nested <- function(r, s) {
  contrib <- contributions(r)
  if (length(s) != 2) stop("'s' must be a pair of numeric values")
  w1_to_r <- weights_transmute(s[1], r)
  w2_to_r <- weights_transmute(s[2], r)
  function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x))) {
    v <- weights_scale(w1_to_r(x, w1)) + weights_scale(w2_to_r(x, w2))
    contrib(x, v)
  }
}

contributions_nested2 <- function(r, s) {
  arithmetic_weights <- weights_transmute(r, 1)
  if (length(s) != 2) stop("'s' must be a pair of numeric values")
  contrib1 <- contributions(s[1])
  contrib2 <- contributions(s[2])
  mean1 <- mean_generalized(s[1])
  mean2 <- mean_generalized(s[2])
  function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x))) {
    m <- c(mean1(x, w1), mean2(x, w2))
    v <- weights_scale(arithmetic_weights(m))
    v[1] * contrib1(x, w1) + v[2] * contrib2(x, w2)
  }
}