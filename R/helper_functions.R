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
`%^%` <- function(e1, e2) {
  if (e2 == 1) {
    e1
  } else if (e2 == 0.5) {
    sqrt(e1)
  } else if (e2 == 0) {
    # making the output the same length as e1 would mean evaluating e1
    # this is also why this isn't a method for ^
    1
  } else if (e2 == -0.5) {
    1 / sqrt(e1)
  } else if (e2 == -1) {
    1 / e1
  } else if (e2 == -2) {
    1 / e1^2
  } else {
    e1^e2
  }
}
