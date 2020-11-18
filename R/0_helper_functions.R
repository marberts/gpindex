# None of these are exported
#---- Argument checking ----
is_T_or_F <- function(x) {
  length(x) == 1 && is.logical(x) && !is.na(x)
}

is_number <- function(x) {
  length(x) == 1 && is.numeric(x) && is.finite(x)
}

is_positive_number <- function(x) {
  is_number(x) && x >= 0
}

all_same_length <- function(...) {
  res <- vapply(list(...), length, numeric(1))
  all(res == res[1])
}

all_numeric <- function(...) {
  all(vapply(list(...), is.numeric, logical(1)))
}

#---- Checks for warnings ----
any_negative <- function(x, y) {
  any(x <= 0, na.rm = T) || any(y <= 0, na.rm = T)
}

small_but_not_zero <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x) < tol && x != 0
}

#---- Wrap-around vector indexing ----
wrap_around <- function(x, i) {
  (i - 1) %% length(x) + 1
}

#---- Custom power operator ----
# There are a variety of optimizations for calculating power/extended means
`%^%` <- function(e1, e2) {
  if (e2 == 1) {
    e1
  } else if (e2 == 0.5) {
    sqrt(e1)
  } else if (e2 == 0) {
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