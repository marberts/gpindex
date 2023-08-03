#---- Transmute weights ----
transmute_weights <- function(r, s) {
  r <- as.numeric(r)
  s <- as.numeric(s)
  gen_mean <- generalized_mean(r)
  ext_mean <- extended_mean(r, s)

  function(x, w = NULL) {
    if (r == s) {
      if (is.null(w)) {
        w <- rep.int(1, length(x))
      }
      w[is.na(x)] <- NA_real_
      w
    } else {
      m <- gen_mean(x, w, na.rm = TRUE)
      if (is.null(w)) {
        v <- ext_mean(x, m)^(r - s)
        attributes(v) <- NULL
      } else {
        v <- w * ext_mean(x, m)^(r - s)
        attributes(v) <- attributes(w)
      }
      v
    }
  }
}

nested_transmute <- function(r1, r2, s, t = c(1, 1)) {
  s_weights <- transmute_weights(r1, s)

  r2 <- as.numeric(r2)
  if (not_finite_pair(r2)) {
    stop("'r2' must be a pair of finite numeric values")
  }

  r_weights1 <- transmute_weights(r2[1L], r1)
  r_weights2 <- transmute_weights(r2[2L], r1)

  t <- as.numeric(t)
  if (length(t) != 2L) {
    stop("'t' must be a pair of numeric values")
  }

  function(x, w1 = NULL, w2 = NULL) {
    if (is.na(t[1L]) && !is.na(t[2L])) {
      w <- scale_weights(r_weights2(x, w2))
    } else if (!is.na(t[1L]) && is.na(t[2L])) {
      w <- scale_weights(r_weights1(x, w1))
    } else {
      v1 <- scale_weights(r_weights1(x, w1))
      v2 <- scale_weights(r_weights2(x, w2))
      # the calculation is wrong if NAs in w1 or w2 propagate
      if (anyNA(w1)) {
        v1[is.na(v1) & !is.na(v2)] <- 0
      }
      if (anyNA(w2)) {
        v2[is.na(v2) & !is.na(v1)] <- 0
      }
      w <- t[1L] * v1 + t[2L] * v2
    }
    s_weights(x, w)
  }
}

nested_transmute2 <- function(r1, r2, s, t = c(1, 1)) {
  s_weights <- transmute_weights(r1, s)

  r2 <- as.numeric(r2)
  if (length(r2) != 2L) {
    stop("'r2' must be a pair of finite numeric values")
  }

  s_weights1 <- transmute_weights(r2[1L], s)
  s_weights2 <- transmute_weights(r2[2L], s)
  mean1 <- generalized_mean(r2[1L])
  mean2 <- generalized_mean(r2[2L])

  t <- as.numeric(t)
  if (length(t) != 2L) {
    stop("'t' must be a pair of numeric values")
  }

  function(x, w1 = NULL, w2 = NULL) {
    m <- c(mean1(x, w1, na.rm = TRUE), mean2(x, w2, na.rm = TRUE))
    v <- s_weights(m, t)
    if (is.na(v[1L]) && !is.na(v[2L])) {
      scale_weights(s_weights2(x, w2))
    } else if (!is.na(v[1L]) && is.na(v[2L])) {
      scale_weights(s_weights1(x, w1))
    } else {
      u1 <- scale_weights(s_weights1(x, w1))
      u2 <- scale_weights(s_weights2(x, w2))
      # the calculation is wrong if NAs in w1 or w2 propagate
      if (anyNA(w1)) {
        u1[is.na(u1) & !is.na(u2)] <- 0
      }
      if (anyNA(w2)) {
        u2[is.na(u2) & !is.na(u1)] <- 0
      }
      v[1L] * u1  + v[2L] * u2
    }
  }
}

#---- Factor weights  ----
factor_weights <- function(r) {
  r <- as.numeric(r)
  if (not_finite_scalar(r)) {
    stop("'r' must be a finite length 1 numeric")
  }

  function(x, w = NULL) {
    if (r == 0) {
      if (is.null(w)) {
        w <- rep.int(1, length(x))
      }
      w[is.na(x)] <- NA_real_
      w
    } else {
      if (is.null(w)) {
        v <- x^r
        attributes(v) <- NULL
      } else {
        v <- w * x^r
        attributes(v) <- attributes(w)
      }
      v
    }
  }
}

update_weights <- factor_weights(1)

#---- Scale weights ----
scale_weights <- function(x) {
  x / sum(x, na.rm = TRUE)
}
