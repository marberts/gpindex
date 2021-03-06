#---- Generalized mean ----
globalVariables(c("x", "w"), "gpindex", add = TRUE)

generalized_mean <- function(r) {
  if (!is_number(r)) {
    stop(gettext("'r' must be a finite length 1 numeric"))
  }
  if (small_but_not_zero(r)) {
    warning(gettext("'r' is very small in absolute value, but not zero; this can give misleading results"))
  }
  # return function
  res <- function(x, w, na.rm = FALSE) {
    # no weights
    if (missing(w)) {
      # removing NAs first means that NaNs for log(negative) are not removed when na.rm = TRUE
      if (na.rm) {
        if (anyNA(x)) {
          x <- x[!is.na(x)]
        }
      }
      # [[2]][[3]][[3]] 
      # weights
    } else {
      if (length(x) != length(w)) {
        stop(gettext("'x' and 'w' must be the same length"))
      }
      if (na.rm) {
        if (anyNA(x) || anyNA(w)) {
          keep <- !(is.na(x) | is.na(w))
          x <- x[keep]
          w <- w[keep]
        }
      }
      # [[2]][[4]][[4]]
    }
  }
  # unweighted calculation
  body(res)[[2]][[3]][[3]] <- if (r == 0) {
    quote(exp(sum(log(x)) / length(x)))
  } else {
    eval(bquote(pow(sum(.(pow(x, r))) / length(x), 1 / r)))
  }
  # weighted calculation
  body(res)[[2]][[4]][[4]] <- if (r == 0) {
    quote(exp(sum(w * log(x)) / sum(w)))
  } else {
    eval(bquote(pow(sum(.(wpow(x, w, r))) / sum(w), 1 / r)))
  }
  # clean up enclosing environment
  enc <- list(r = r)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

#---- Pythagorean means ----
arithmetic_mean <- generalized_mean(1)

geometric_mean <- generalized_mean(0)

harmonic_mean <- generalized_mean(-1)

#---- Extended mean ----
globalVariables(c("a", "b"), "gpindex", add = TRUE)

extended_mean <- function(r, s) {
  if (!is_number(r) || !is_number(s)) {
    stop(gettext("'r' and 's' must be finite length 1 numerics"))
  }
  if (small_but_not_zero(r)) {
    warning(gettext("'r' is very small in absolute value, but not zero; this can give misleading results"))
  }
  if (small_but_not_zero(s)) {
    warning(gettext("'s' is very small in absolute value, but not zero; this can give misleading results"))
  }
  if (small_but_not_zero(r - s)) {
    warning(gettext("'r' and 's' are very close in value, but not equal; this can give misleading results"))
  }
  # return function
  res <- function(a, b, tol = .Machine$double.eps^0.5) {
    res # placeholder
    # set output to a when a == b
    loc <- which(abs(a - b) <= tol)
    res[loc] <- a[wrap_around(a, loc)]
    res
  }
  expr <- if (r == 0 && s == 0) {
    quote(sqrt(a * b))
  } else if (r == 0) {
    # ((a^s - b^s) / log(a / b) / s)^(1 / s)
    z <- bquote((.(pow(a, s)) - .(pow(b, s))) / log(a / b))
    if (s != 1) {
      eval(bquote(pow(.(z) / s, 1 / s)))
    } else {
      z
    }
  } else if (s == 0) {
    # ((a^r - b^r) / log(a / b) / r)^(1 / r)
    z <- bquote((.(pow(a, r)) - .(pow(b, r))) / log(a / b))
    if (r != 1) {
      eval(bquote(pow(.(z) / r, 1 / r)))
    } else {
      z
    }
  } else if (r == s) {
    # exp((a^r * log(a) - b^r * log(b)) / (a^r - b^r) - 1 / r)
    bquote(exp((.(pow(a, r)) * log(a) - .(pow(b, r)) * log(b)) / 
                 (.(pow(a, r)) - .(pow(b, r))) - 1 / r))
  } else {
    # ((a^s - b^s) / (a^r - b^r) * r / s)^(1 / (s - r))
    z <- bquote((.(pow(a, s)) - .(pow(b, s))) / (.(pow(a, r)) - .(pow(b, r))))
    if (r == 1) {
      eval(bquote(pow(.(z) / s, 1 / (s - 1))))
    } else if (s == 1) {
      eval(bquote(pow(.(z) * r, 1 / (1 - r))))
    } else {
      eval(bquote(pow(.(z) * (r / s), 1 / (s - r))))
    }
  }
  body(res)[[2]] <- call("<-", quote(res), expr)
  # clean up enclosing environment
  enc <- list(r = r, s = s)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

#---- Logarithmic means ----
generalized_logmean <- function(r) {
  extended_mean(r, 1)
}

logmean <- generalized_logmean(0)

#---- Lehmer mean ----
lehmer_mean <- function(r) {
  if (!is_number(r)) {
    stop(gettext("'r' must be a finite length 1 numeric"))
  }
  # return function
  res <- function(x, w, na.rm = FALSE) {
    if (missing(w)) {
      # [[2]][[3]]
    } else {
      # [[2]][[4]]
    }
  }
  body(res)[[2]][[3]] <- if (r != 1) {
    call("arithmetic_mean", quote(x), pow(x, r - 1), quote(na.rm))
  } else {
    call("arithmetic_mean", quote(x), na.rm = quote(na.rm))
  }
  body(res)[[2]][[4]] <- call("arithmetic_mean", quote(x), wpow(x, w, r - 1), quote(na.rm))
  # clean up enclosing environment
  enc <- list(r = r)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

contraharmonic_mean <- lehmer_mean(2)

#---- Nested mean ----
nested_mean <- function(r, s, t = c(1, 1)) {
  outer_mean <- generalized_mean(r)
  if (length(s) != 2) {
    stop(gettext("'s' must be a pair of numeric values"))
  }
  inner_mean1 <- generalized_mean(s[1])
  inner_mean2 <- generalized_mean(s[2])
  if (length(t) != 2 || !is.numeric(t)) {
    stop(gettext("'t' must be a pair of numeric values"))
  }
  t <- as.numeric(t) # strip any attributes
  # return function
  function(x, w1, w2, na.rm = FALSE) {
    x <- c(inner_mean1(x, w1, na.rm), inner_mean2(x, w2, na.rm))
    outer_mean(x, t, na.rm)
  }
}

fisher_mean <- nested_mean(0, c(1, -1))
