# None of these functions are exported
#---- Argument checking ----
is_number <- function(x) {
  length(x) == 1 && is.numeric(x) && is.finite(x)
}

same_length <- function(...) {
  res <- lengths(list(...))
  all(res == res[1])
}

#---- Checks for warnings ----
any_negative <- function (...) {
  min(..., 1, na.rm = TRUE) <= 0 # the 1 stops the warnings with length-0 inputs
}

small_but_not_zero <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x) < tol && x != 0
}

#---- Wrap-around vector indexing ----
wrap_around <- function(x, i) {
  (i - 1) %% length(x) + 1
}

#---- Total value ----
v <- function(p, q) {
  sum(p * q, na.rm = TRUE)
}

#---- Custom power operator ----
# There are a variety of optimizations for calculating power/extended means
pow <- function(x, r) {
  if (r == 1) {
    substitute(x)
  } else if (r == 0.5) {
    substitute(sqrt(x))
  } else if (r == -0.5) {
    substitute(1 / sqrt(x))
  } else if (r == -1) {
    substitute(1 / x)
  } else if (r < 0) {
    substitute(1 / x^abs(r))
  } else {
    substitute(x^r)
  }
}

wpow <- function(x, w, r) {
  if (r == 1) {
    substitute(w * x)
  } else if (r == 0.5) {
    substitute(w * sqrt(x))
  } else if (r == 0) {
    substitute(w)
  } else if (r == -0.5) {
    substitute(w / sqrt(x))
  } else if (r == -1) {
    substitute(w / x)
  } else if (r < 0) {
    substitute(w / x^abs(r))
  } else {
    substitute(w * x^r)
  }
}
