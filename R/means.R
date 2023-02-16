#---- Generalized mean ----
generalized_mean <- function(r) {
  if (not_number(r)) {
    stop(gettext("'r' must be a finite length 1 numeric"))
  }
  
  function(x, w = NULL, na.rm = FALSE) {
    if (is.null(w)) {
      if (na.rm && anyNA(x)) {
        x <- x[!is.na(x)]
      }
      if (r != 0 ){
        (sum(x^r) / length(x))^(1 / r)
      } else {
        exp(sum(log(x)) / length(x))
      }
    } else {
      if (length(x) != length(w)) {
        stop(gettext("'x' and 'w' must be the same length"))
      }
      if (na.rm && (anyNA(x) || anyNA(w))) {
        keep <- !(is.na(x) | is.na(w))
        x <- x[keep]
        w <- w[keep]
      }
      if (r != 0 ){
        (sum(x^r * w) / sum(w))^(1 / r)
      } else {
        exp(sum(log(x) * w) / sum(w))
      }
    }
  }
}

arithmetic_mean <- generalized_mean(1)

geometric_mean <- generalized_mean(0)

harmonic_mean <- generalized_mean(-1)

#---- Extended mean ----
extended_mean <- function(r, s) {
  if (not_number(r)) {
    stop(gettext("'r' must be a finite length 1 numeric"))
  }
  if (not_number(s)) {
    stop(gettext("'s' must be a finite length 1 numeric"))
  }
  
  function(a, b, tol = .Machine$double.eps^0.5) {
    res <- if (r == 0 && s == 0) {
      sqrt(a * b)
    } else if (r == 0) {
      ((a^s - b^s) / log(a / b) / s)^(1 / s)
    } else if (s == 0) {
      ((a^r - b^r) / log(a / b) / r)^(1 / r)
    } else if (r == s) {
      exp((a^r * log(a) - b^r * log(b)) / (a^r - b^r) - 1 / r)
    } else {
      ((a^s - b^s) / (a^r - b^r) * r / s)^(1 / (s - r))
    }
    # set output to a when a == b
    i <- which(abs(a - b) <= tol)
    res[i] <- a[(i - 1L) %% length(a) + 1L]
    res
  }
}

generalized_logmean <- function(r) {
  extended_mean(r, 1)
}

logmean <- generalized_logmean(0)

#---- Lehmer mean ----
lehmer_mean <- function(r) {
  if (not_number(r)) {
    stop(gettext("'r' must be a finite length 1 numeric"))
  }
  
  function(x, w = NULL, na.rm = FALSE) {
    v <- if (is.null(w)) x^(r - 1) else x^(r - 1) * w
    arithmetic_mean(x, v, na.rm = na.rm)
  }
}

contraharmonic_mean <- lehmer_mean(2)

#---- Nested mean ----
nested_mean <- function(r1, r2, t = c(1, 1)) {
  outer_mean <- generalized_mean(r1)
  if (length(r2) != 2L) {
    stop(gettext("'r2' must be a pair of numeric values"))
  }
  inner_mean1 <- generalized_mean(r2[1L])
  inner_mean2 <- generalized_mean(r2[2L])
  if (length(t) != 2L || !is.numeric(t)) {
    stop(gettext("'t' must be a pair of numeric values"))
  }
  t <- as.numeric(t) # strip any attributes

  function(x, w1 = NULL, w2 = NULL, na.rm = FALSE) {
    x <- c(inner_mean1(x, w1, na.rm), inner_mean2(x, w2, na.rm))
    outer_mean(x, t, na.rm)
  }
}

fisher_mean <- nested_mean(0, c(1, -1))
