#---- Price utilities----
offset_period <- function(f) {
  f <- match.fun(f)
  # return function
  function(period, product = gl(1, length(period))) {
    if (length(period) != length(product)) {
      stop(gettext("'period' and 'product' must be the same length"))
    }
    if (!length(period)) return(integer(0))
    period <- as.factor(period)
    product <- as.factor(product)
    attributes(product) <- NULL # matching is faster on factor codes
    product <- split(product, period)
    if (max(vapply(product, anyDuplicated, numeric(1), incomparables = NA))) {
      warning(gettext("there are duplicated period-product pairs"))
    }
    m <- .mapply(match, list(product, f(product)), list(incomparables = NA))
    res <- split(seq_along(period), period)
    unsplit(.mapply(`[`, list(f(res), m), list()), period)
  }
}

back_period <- offset_period(function(x) x[c(1L, seq_len(length(x) - 1))])

base_period <- offset_period(function(x) x[1L])

offset_price <- function(f) {
  f <- match.fun(f)
  # return function
  function(x, period, product = gl(1, length(x))) {
    if (different_lengths(x, period, product)) {
      stop(gettext("all arguments must be the same length"))
    }
    res <- x[f(period, product)]
    attributes(res) <- attributes(x)
    res
  }
}

back_price <- offset_price(back_period)

base_price <- offset_price(base_period)

#---- Outlier utilities ----
# all of these start with as.numeric() to strip attributes
quartile_method <- function(x, cu = 2.5, cl = cu, a = 0, type = 7) {
  x <- as.numeric(x)
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE, na.rm = TRUE, type = type)
  x <- x - q[2]
  u <- cu * pmax(q[3] - q[2], abs(a * q[2]))
  l <- -cl * pmax(q[2] - q[1], abs(a * q[2]))
  x > u | x < l
}

resistant_fences <- function(x, cu = 2.5, cl = cu, a = 0, type = 7) {
  x <- as.numeric(x)
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE, na.rm = TRUE, type = type)
  iqr <- pmax(q[3] - q[1], abs(a * q[2]))
  u <- q[3] + cu * iqr
  l <- q[1] - cl * iqr
  x > u | x < l
}

robust_z <- function(x, cu = 2.5, cl = cu) {
  x <- as.numeric(x)
  med <- median(x, na.rm = TRUE)
  s <- mad(x, na.rm = TRUE)
  x <- x - med
  u <- cu * s
  l <- -cl * s
  x > u | x < l
}

fixed_cutoff <- function(x, cu = 2.5, cl = 1 / cu) {
  x <- as.numeric(x)
  x > cu | x < cl
}

tukey_algorithm <- function(x, cu = 2.5, cl = cu, type = 7) {
  x <- as.numeric(x)
  q <- quantile(x, c(0.05, 0.95), names = FALSE, na.rm = TRUE, type = type)
  tail <- x < q[1] | x > q[2]
  ts <- x[x != 1 & !tail]
  if (!length(ts)) return(tail)
  m <- mean(ts, na.rm = TRUE)
  x <- x - m
  u <- cu * (mean(ts[ts >= m], na.rm = TRUE) - m) # in some versions m is the median
  l <- -cl * (m - mean(ts[ts <= m], na.rm = TRUE))
  x > u | x < l | tail
}

hb_transform <- function(x) {
  x <- as.numeric(x)
  med <- median(x, na.rm = TRUE)
  res <- 1 - med / x
  gemed <- x >= med
  res[gemed] <- x[gemed] / med - 1
  res
}