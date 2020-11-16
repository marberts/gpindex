#---- Transmute weights ----
weights_transmute <- function(r, s) {
  generalized_mean <- mean_generalized(r)
  extended_mean <- mean_extended(r, s)
  # return function
  function(x, w = rep(1, length(x))) {
    res <- w * extended_mean(x, generalized_mean(x, w, na.rm = TRUE)) %^% (r - s)
    # make sure NAs propagate
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
    # make sure NAs propagate
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

contributions_nested <- function(r1, r2, w1 = rep(1, length(r2))) {
  stopifnot(is_number(r1), 
            is.list(r2), 
            vapply(r2, is_number, logical(1)), 
            is.numeric(w1),
            all_same_length(r2, w1))
  function(x, w = rep(list(rep(1, length(x))), length(r2))) {
    stopifnot(is.numeric(x), 
              is.list(w),
              all_same_length(r2, w),
              vapply(w, is.numeric, logical(1)),
              do.call(all_same_length, c(list(x), w)))
    means <- lapply(r2, mean_generalized)
    m <- vapply(seq_along(means), function(i) means[[i]](x, w[[i]]), numeric(1))
    v1 <- weights_scale(weights_transmute(r1, 1)(m, w1))
    contribs <- lapply(r2, contributions)
    v2 <- lapply(seq_along(contribs), function(i) contribs[[i]](x, w[[i]]))
    unlist(Reduce("+", Map("*", v1, v2)))
  }
}