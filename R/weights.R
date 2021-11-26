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
  body(res)[[2]][[3]] <- if (r == s) {
    # make sure NAs carry on
    quote(replace(rep(1, length(x)), is.na(x), NA_real_))
  } else {
    pow(ext_mean(x, gen_mean(x, na.rm = TRUE)), r - s)
  }
  # weighted calculation
  body(res)[[2]][[4]] <- if (r == s) {
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
  body(res)[[2]][[3]] <- if (r == 0) {
    # make sure NAs carry on
    quote(replace(rep(1, length(x)), is.na(x), NA_real_))
  } else {
    pow(x, r)
  }
  # weighted calculation
  body(res)[[2]][[4]] <- if (r == 0) {
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

#---- Contributions ----
contributions <- function(r) {
  arithmetic_weights <- transmute_weights(r, 1)
  # return function
  res <- function(x, w) {
    scale_weights(arithmetic_weights(x, w)) * (x - 1)
  }
  # clean up enclosing environment
  enc <- list(arithmetic_weights = arithmetic_weights)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

arithmetic_contributions <- contributions(1)

geometric_contributions <- contributions(0)

harmonic_contributions <- contributions(-1)

#---- Nested contributions ----
nested_contributions <- function(r, s, t = c(1, 1)) {
  contrib <- contributions(r)
  if (length(s) != 2) {
    stop(gettext("'s' must be a pair of numeric values"))
  }
  r_weights1 <- transmute_weights(s[1], r)
  r_weights2 <- transmute_weights(s[2], r)
  if (length(t) != 2 || !is.numeric(t)) {
    stop(gettext("'t' must be a pair of numeric values"))
  }
  t <- as.numeric(t) # strip attributes
  # return function
  res <- function(x, w1, w2) {
    v1 <- scale_weights(r_weights1(x, w1))
    v2 <- scale_weights(r_weights2(x, w2))
    # the calculation is wrong if NAs in w1 or w2 propagate
    if (!missing(w1) && anyNA(w1)) v1[is.na(v1) & !is.na(v2)] <- 0
    if (!missing(w2) && anyNA(w2)) v2[is.na(v2) & !is.na(v1)] <- 0
    # same for t
    t[is.na(t) & !rev(is.na(t))] <- 0
    contrib(x, t[1] * v1 + t[2] * v2)
  }
  # clean up enclosing environment
  enc <- list(contrib = contrib,
              r_weights1 = r_weights1,
              r_weights2 = r_weights2,
              t = t)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

nested_contributions2 <- function(r, s, t = c(1, 1)) {
  arithmetic_weights <- transmute_weights(r, 1)
  if (length(s) != 2) {
    stop(gettext("'s' must be a pair of numeric values"))
  }
  contrib1 <- contributions(s[1])
  contrib2 <- contributions(s[2])
  mean1 <- generalized_mean(s[1])
  mean2 <- generalized_mean(s[2])
  if (length(t) != 2 || !is.numeric(t)) {
    stop(gettext("'t' must be a pair of numeric values"))
  }
  t <- as.numeric(t) # strip attributes
  # return function
  res <- function(x, w1, w2) {
    m <- c(mean1(x, w1, na.rm = TRUE), mean2(x, w2, na.rm = TRUE))
    v <- scale_weights(arithmetic_weights(m, t))
    u1 <- contrib1(x, w1)
    u2 <- contrib2(x, w2)
    # the calculation is wrong if NAs in w1 or w2 propagate
    if (!missing(w1) && anyNA(w1)) u1[is.na(u1) & !is.na(u2)] <- 0
    if (!missing(w2) && anyNA(w2)) u2[is.na(u2) & !is.na(u1)] <- 0
    # same for v
    v[is.na(v) & !rev(is.na(v))] <- 0
    v[1] * u1  + v[2] * u2 
  }
  # clean up enclosing environment
  enc <- list(arithmetic_weights = arithmetic_weights,
              contrib1 = contrib1,
              contrib2 = contrib2,
              mean1 = mean1,
              mean2 = mean2,
              t = t)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

fisher_contributions <- nested_contributions(0, c(1, -1))

fisher_contributions2 <- nested_contributions2(0, c(1, -1))