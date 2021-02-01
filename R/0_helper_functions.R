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
any_negative <- function(x, y) {
  any(x <= 0, na.rm = TRUE) || any(y <= 0, na.rm = TRUE)
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

#---- Unit weights ----
# The uw class optimizes weighted means with equal weights.
# It improves performance for large (>= 1e6) vectors, but I'm not sure
# it's worth the extra complexity

# unit_weights <- function(x) structure(length(x), class = "uw")
# 
# sum.uw <- function(x, ..., na.rm = FALSE) sum(..., na.rm = na.rm) + unclass(x)
# 
# length.uw <- function(x) unclass(x)
# 
# `*.uw` <- function(e1, e2) {
#   lhs <- nzchar(.Method[1])
#   rhs <- nzchar(.Method[2])
#   if (rhs && lhs) {
#     structure(max(e1, e2), class = "uw")
#   } else if (lhs) {
#     if (length(e2) < e1) rep_len(e2, e1) else e2
#   } else {
#     if (length(e1) < e2) rep_len(e1, e2) else e1
#   }
# }
# 
# `[.uw` <- function(x, i) structure(length((1L)[i]), class = "uw")