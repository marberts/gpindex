#---- Transmute weights ----
weights_transmute <- function(r, s) {
  generalized_mean <- mean_generalized(r)
  extended_mean <- mean_extended(r, s)
  # return function
  function(x, w = rep(1, length(x))) {
    m <- generalized_mean(x, w, na.rm = TRUE)
    res <- w * extended_mean(x, m) %^% (r - s)
    # make sure NAs propagate
    if (r == s) res[is.na(x) & !is.na(w)] <- NA
    res
  }
}

#---- Factor weights  ----
weights_factor <- function(r) {
  stopifnot("'r' must be a finite length 1 numeric vector" = length1(r, "numeric"))
  # return function
  function(x, w = rep(1, length(x))) {
    stopifnot("'x' and 'w' must be numeric vectors" = is_numeric(x, w),
              "'x' and 'w' must be the same length" = same_length(x, w))
    res <- w * x %^% r
    # make sure NAs propagate
    if (r == 0) res[is.na(x) & !is.na(w)] <- NA
    res
  }
}

weights_update <- weights_factor(1)

#---- Scale weights ----
weights_scale <- function(x) {
  stopifnot("'x' must be a numeric vector" = is_numeric(x))
  x / sum(x, na.rm = TRUE)
}

#---- Contributions ----
contributions <- function(r) {
  arithmetic_weights <- weights_transmute(r, 1)
  function(x, w = rep(1, length(x))) {
    weights_scale(arithmetic_weights(x, w)) * (x - 1)
  }
}

contributions_geometric <- contributions(0)

contributions_harmonic <- contributions(-1)