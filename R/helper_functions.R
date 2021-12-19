# None of these functions are exported
#---- Argument checking ----
not_number <- function(x) {
  length(x) != 1L || !is.numeric(x) || !is.finite(x)
}

different_lengths <- function(...) {
  res <- lengths(list(...))
  any(res != res[1L])
}

small_but_not_zero <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x) < tol && x != 0
}

#---- Custom power operator ----
# There are a variety of optimizations for calculating power/extended means
# These are important to keep the Pythagorean calculations relatively fast
pow <- function(x, r) {
  if (r == 1) {
    substitute(x)
  } else if (r == -1) {
    substitute(1 / x)
  } else if (r == -2) {
    substitute(1 / x^2)
  } else {
    eval(bquote(substitute(x^.(r))))
  }
}

wpow <- function(x, w, r) {
  if (r == 1) {
    substitute(w * x)
  } else if (r == 0) {
    substitute(w)
  } else if (r == -1) {
    substitute(w / x)
  } else if (r == -2) {
    substitute(w / x^2)
  } else {
    eval(bquote(substitute(w * x^.(r))))
  }
}

#---- Geks helpers ----
to_scalar <- function(x) {
  trunc(as.numeric(x[1L]))
}

rolling_window <- function(n, window) {
  steps <- seq_len(n - window + 1)
  Map(seq, from = steps, length.out = window)
}