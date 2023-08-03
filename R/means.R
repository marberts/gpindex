#---- Generalized mean ----
generalized_mean <- function(r) {
  r <- as.numeric(r)
  if (not_finite_scalar(r)) {
    stop("'r' must be a finite length 1 numeric")
  }

  function(x, w = NULL, na.rm = FALSE) {
    if (is.null(w)) {
      if (na.rm && anyNA(x)) {
        x <- x[!is.na(x)]
      }
      if (r == 0) {
        exp(sum(log(x)) / length(x))
      } else {
        (sum(x^r) / length(x))^(1 / r)
      }
    } else {
      if (length(x) != length(w)) {
        stop("'x' and 'w' must be the same length")
      }
      if (na.rm && (anyNA(x) || anyNA(w))) {
        keep <- !(is.na(x) | is.na(w))
        x <- x[keep]
        w <- w[keep]
      }
      if (r == 0) {
        exp(sum(log(x) * w) / sum(w))
      } else {
        (sum(x^r * w) / sum(w))^(1 / r)
      }
    }
  }
}

arithmetic_mean <- generalized_mean(1)

geometric_mean <- generalized_mean(0)

harmonic_mean <- generalized_mean(-1)

#---- Extended mean ----
extended_mean <- function(r, s) {
  r <- as.numeric(r)
  s <- as.numeric(s)
  if (not_finite_scalar(r)) {
    stop("'r' must be a finite length 1 numeric")
  }
  if (not_finite_scalar(s)) {
    stop("'s' must be a finite length 1 numeric")
  }

  function(a, b, tol = .Machine$double.eps^0.5) {
    if (r == 0 && s == 0) {
      res <- sqrt(a * b)
    } else if (r == 0) {
      res <- ((a^s - b^s) / log(a / b) / s)^(1 / s)
    } else if (s == 0) {
      res <- ((a^r - b^r) / log(a / b) / r)^(1 / r)
    } else if (r == s) {
      res <- exp((a^r * log(a) - b^r * log(b)) / (a^r - b^r) - 1 / r)
    } else {
      res <- ((a^s - b^s) / (a^r - b^r) * r / s)^(1 / (s - r))
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
  r <- as.numeric(r)
  if (not_finite_scalar(r)) {
    stop("'r' must be a finite length 1 numeric")
  }

  function(x, w = NULL, na.rm = FALSE) {
    v <- x^(r - 1)
    if (!is.null(w)) {
      v <- v * w
    }
    arithmetic_mean(x, v, na.rm = na.rm)
  }
}

contraharmonic_mean <- lehmer_mean(2)

#---- Nested mean ----
nested_mean <- function(r1, r2, t = c(1, 1)) {
  outer_mean <- generalized_mean(r1)

  r2 <- as.numeric(r2)
  if (not_finite_pair(r2)) {
    stop("'r2' must be a pair of finite numeric values")
  }

  inner_mean1 <- generalized_mean(r2[1L])
  inner_mean2 <- generalized_mean(r2[2L])

  t <- as.numeric(t)
  if (length(t) != 2) {
    stop("'t' must be a pair of numeric values")
  }

  function(x, w1 = NULL, w2 = NULL, na.rm = FALSE) {
    x <- c(inner_mean1(x, w1, na.rm), inner_mean2(x, w2, na.rm))
    outer_mean(x, t, na.rm)
  }
}

fisher_mean <- nested_mean(0, c(1, -1))
