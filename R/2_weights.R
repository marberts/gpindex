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

#---- Nested contributions ----
contributions_nested <- function(r, s) {
  contrib <- contributions(r)
  if (length(s) != 2) stop("'s' must be a pair of numeric values")
  r_weights <- lapply(s, weights_transmute, r)
  function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x))) {
    if (anyNA(w1) || anyNA(w2)) {
      warning("nested contributions are not well defined with NA weights")
    }
    v <- weights_scale(r_weights[[1]](x, w1)) + weights_scale(r_weights[[2]](x, w2))
    contrib(x, v)
  }
}

contributions_nested2 <- function(r, s) {
  arithmetic_weights <- weights_transmute(r, 1)
  if (length(s) != 2) stop("'s' must be a pair of numeric values")
  contrib <- lapply(s, contributions)
  mean <- lapply(s, mean_generalized)
  function(x, w1 = rep(1, length(x)), w2 = rep(1, length(x))) {
    if (anyNA(w1) || anyNA(w2)) {
      warning("nested contributions are not well defined with NA weights")
    }
    m <- c(mean[[1]](x, w1, na.rm = TRUE), mean[[2]](x, w2, na.rm = TRUE))
    v <- weights_scale(arithmetic_weights(m))
    v[1] * contrib[[1]](x, w1) + v[2] * contrib[[2]](x, w2)
  }
}