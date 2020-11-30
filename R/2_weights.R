#---- Transmute weights ----
weights_transmute <- function(r, s) {
  generalized_mean <- mean_generalized(r)
  extended_mean <- mean_extended(r, s)
  # return function
  function(x, w = rep(1, length(x))) {
    res <- w * extended_mean(x, generalized_mean(x, w, na.rm = TRUE)) %^% (r - s)
    # make sure NAs propagate to ensure weights scale correctly with NAs in x
    replace(res, if (r == s) is.na(x) & !is.na(w), NA)
  }
}

#---- Factor weights  ----
weights_factor <- function(r) {
  stopifnot("'r' must be a finite length 1 numeric" = is_number(r))
  # return function
  function(x, w = rep(1, length(x))) {
    stopifnot("'x' and 'w' must be numeric vectors" = all_numeric(x, w),
              "'x' and 'w' must be the same length" = all_same_length(x, w))
    res <- w * x %^% r
    # make sure NAs propagate to ensure chaining works correctly with NAs in x
    replace(res, if (r == 0) is.na(x) & !is.na(w), NA)
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

contributions_nested <- function(r1, r2, w1 = rep(1L, length(r2))) {
  stopifnot("'r1' must be a finite length 1 numeric" = is_number(r1),
            "'r2' and 'w1' must be a numeric vectors" = all_numeric(r2, w1),
            "'r2' and 'w1' must be the same length" = all_same_length(r2, w1))
  function(x, w = rep(list(rep(1, length(x))), length(r2))) {
    stopifnot("'x' must be a numeric vector" = is.numeric(x),
              "'w' must be a list" = is.list(w),
              "'r2' and 'w' must be the same length" = all_same_length(r2, w),
              "Each element of 'w' must be a numeric vector" = do.call(all_numeric, w),
              "Each element of 'w' must be the same length as 'x'" = do.call(all_same_length, c(list(x), w)))
    means <- lapply(r2, mean_generalized)
    contribs <- lapply(r2, contributions)
    ind <- seq_along(r2)
    m <- vapply(ind, function(i) means[[i]](x, w[[i]]), numeric(1))
    v2 <- lapply(ind, function(i) contribs[[i]](x, w[[i]]))
    v1 <- weights_scale(weights_transmute(r1, 1)(m, w1))
    unlist(Reduce("+", Map("*", v1, v2)))
  }
}