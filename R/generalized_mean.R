generalized_mean <- function(x, w, r, na.rm = FALSE) {
  # check input
  stopifnot(
    is.numeric(x) || is.logical(x), # x should be numeric or logical
    missing(w) || (is.numeric(w) || is.logical(w)), # weights should be numeric or logical 
    length(r) == 1L, is.numeric(r), is.finite(r), # r must be a length 1 numeric
    length(na.rm) == 1L, is.logical(na.rm) # na.rm must be a length 1 logical
  )
  # geomean if r = 0
  if (r == 0) { 
    exp(stats::weighted.mean(log(x), w, na.rm = na.rm))
  # r = +-1 cases are faster on their own without needless ^1
  } else if (abs(r) == 1) { 
    # arithmetic mean if r = 1
    if (r == 1) { 
      stats::weighted.mean(x, w, na.rm = na.rm)
    # harmonic mean if r = -1
    } else { 
      1 / stats::weighted.mean(1 / x, w, na.rm = na.rm)
    }
  # generalized mean otherwise
  # if r < 0 then 1 / x^r is faster than x^(-r)
  } else if (r < 0) { 
    1 / (stats::weighted.mean((1 / x^abs(r)), w, na.rm = na.rm))^(1 / abs(r)) 
  } else {
    (stats::weighted.mean(x^r, w, na.rm = na.rm))^(1 / r)
  }
}

geometric_mean <- function(x, w, na.rm = FALSE) generalized_mean(x, w, 0, na.rm)

arithmetic_mean <- function(x, w, na.rm = FALSE) generalized_mean(x, w, 1, na.rm)

harmonic_mean <- function(x, w, na.rm = FALSE) generalized_mean(x, w, r = -1, na.rm)