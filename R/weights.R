#---- Transmute weights ----
weights_transmute <- function(r, s) {
  generalized_mean <- mean_generalized(r)
  extended_mean <- mean_extended(r, s)
  # return function
  function(x, w = rep(1, length(x))) {
    w * extended_mean(x, generalized_mean(x, w, na.rm = TRUE)) %^% (r - s)
  }
}

#---- Common cases for transmute ----
weights_g2a <- weights_transmute(0, 1)

weights_h2a <- weights_transmute(-1, 1)

#---- Factor weights  ----
weights_factor <- function(r) {
  stopifnot("'r' must be a finite length 1 numeric vector" = length1(r, "numeric"))
  # return function
  function(x, w = rep(1, length(x))) {
    stopifnot("'x' and 'w' must be numeric vectors" = is_numeric(x, w),
              "'x' and 'w' must be the same length" = same_length(x, w))
    res <- w * x %^% r
    # make sure NAs propegate
    if (r == 0) res[is.na(x) & !is.na(w)] <- NA
    res
  }
}

#---- Price-update weights ----
weights_update <- weights_factor(1)

#---- Scale weights ----
weights_scale <- function(x) {
  stopifnot("'w' must be a numeric vector" = is_numeric(x))
  x / sum(x, na.rm = TRUE)
}