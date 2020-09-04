#---- Argument checking ----
length1 <- function(x, mode) {
  length(x) == 1L && is.vector(x, mode) && is.finite(x)
}

same_length <- function(...) {
  res <- vapply(list(...), length, numeric(1))
  all(res == res[1])
}

is_numeric <- function(...) {
  res <- vapply(list(...), is.vector, logical(1), mode = "numeric")
  all(res)
}

#---- Custom pow ----
# There are a variety of optimizations for calculating power/extended means
`%^%` <- function(e1, e2) {
  e1 <- substitute(e1)
  res <- if (e2 == 1) {
    e1
  } else if (e2 == 0.5) {
    call("sqrt", e1)
  } else if (e2 == 0) {
    call("(", 1)
  } else if (e2 == -0.5) {
    call("/", 1, call("sqrt", e1))
  } else if (e2 == -1) {
    call("/", 1, e1)
  } else if (e2 == -2) {
    call("/", 1, call("^", e1, 2))
  } else {
    call("^", e1, e2)
  }
  eval(res, parent.frame())
}