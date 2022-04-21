#---- Transmute weights ----
transmute_weights <- function(r, s) {
  gen_mean <- generalized_mean(r)
  ext_mean <- extended_mean(r, s)
  # return function
  res <- function(x, w) {
    if (missing(w)) {
      # [[2]][[3]] unweighted calculation
    } else {
      # [[2]][[4]] weighted calculation
    }
  }
  # unweighted calculation
  body(res)[[c(2L, 3L)]] <- if (r == s) {
    # make sure NAs carry on
    quote(replace(rep(1, length(x)), is.na(x), NA_real_))
  } else {
    pow(ext_mean(x, gen_mean(x, na.rm = TRUE)), r - s)
  }
  # weighted calculation
  body(res)[[c(2L, 4L)]] <- if (r == s) {
    # make sure NAs carry on
    quote({w[is.na(x)] <- NA; w})
  } else {
    wpow(ext_mean(x, gen_mean(x, w, na.rm = TRUE)), w, r - s)
  }
  # clean up enclosing environment
  enc <- list(gen_mean = gen_mean, ext_mean = ext_mean)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

nested_transmute <- function(r1, r2, s, t = c(1, 1)) {
  s_weights <- transmute_weights(r1, s)
  if (length(r2) != 2L) {
    stop(gettext("'r2' must be a pair of numeric values"))
  }
  r_weights1 <- transmute_weights(r2[1L], r1)
  r_weights2 <- transmute_weights(r2[2L], r1)
  if (length(t) != 2L || !is.numeric(t)) {
    stop(gettext("'t' must be a pair of numeric values"))
  }
  t <- as.numeric(t) # strip attributes
  # return function
  res <- function(x, w1, w2) {
    w <- if (is.na(t[1L]) && !is.na(t[2L])) {
      scale_weights(r_weights2(x, w2))
    } else if (!is.na(t[1L]) && is.na(t[2L])) {
      scale_weights(r_weights1(x, w1))
    } else {
      v1 <- scale_weights(r_weights1(x, w1))
      v2 <- scale_weights(r_weights2(x, w2))
      # the calculation is wrong if NAs in w1 or w2 propagate
      if (!missing(w1) && anyNA(w1)) {
        v1[is.na(v1) & !is.na(v2)] <- 0
      }
      if (!missing(w2) && anyNA(w2)) {
        v2[is.na(v2) & !is.na(v1)] <- 0
      }
      t[1L] * v1 + t[2L] * v2
    }
    s_weights(x, w)
  }
  # clean up enclosing environment
  enc <- list(s_weights = s_weights,
              r_weights1 = r_weights1,
              r_weights2 = r_weights2,
              t = t)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

nested_transmute2 <- function(r1, r2, s, t = c(1, 1)) {
  s_weights <- transmute_weights(r1, s)
  if (length(r2) != 2L) {
    stop(gettext("'r2' must be a pair of numeric values"))
  }
  s_weights1 <- transmute_weights(r2[1L], s)
  s_weights2 <- transmute_weights(r2[2L], s)
  mean1 <- generalized_mean(r2[1L])
  mean2 <- generalized_mean(r2[2L])
  if (length(t) != 2L || !is.numeric(t)) {
    stop(gettext("'t' must be a pair of numeric values"))
  }
  t <- as.numeric(t) # strip attributes
  # return function
  res <- function(x, w1, w2) {
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
      if (!missing(w1) && anyNA(w1)) {
        u1[is.na(u1) & !is.na(u2)] <- 0
      }
      if (!missing(w2) && anyNA(w2)) {
        u2[is.na(u2) & !is.na(u1)] <- 0
      }
      v[1L] * u1  + v[2L] * u2 
    }
  }
  # clean up enclosing environment
  enc <- list(s_weights = s_weights,
              s_weights1 = s_weights1,
              s_weights2 = s_weights2,
              mean1 = mean1,
              mean2 = mean2,
              t = t)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

#---- Factor weights  ----
factor_weights <- function(r) {
  if (not_number(r)) {
    stop(gettext("'r' must be a finite length 1 numeric"))
  }
  # return function
  res <- function(x, w) {
    if (missing(w)) {
      # [[2]][[3]] unweighted calculation
    } else {
      # [[2]][[4]] weighted calculation
    }
  }
  # unweighted calculation
  body(res)[[c(2L, 3L)]] <- if (r == 0) {
    # make sure NAs carry on
    quote(replace(rep(1, length(x)), is.na(x), NA_real_))
  } else {
    pow(x, r)
  }
  # weighted calculation
  body(res)[[c(2L, 4L)]] <- if (r == 0) {
    # make sure NAs carry on
    quote({w[is.na(x)] <- NA; w})
  } else {
    wpow(x, w, r)
  }
  # clean up enclosing environment
  environment(res) <- getNamespace("gpindex")
  res
}

update_weights <- factor_weights(1)

#---- Scale weights ----
scale_weights <- function(x) {
  x / sum(x, na.rm = TRUE)
}
