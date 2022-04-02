# having all of these start with as.numeric() strips attributes and ensures the return
# value is a logical vector

quartile_method <- function(x, cu = 2.5, cl = cu, a = 0, type = 7) {
  x <- as.numeric(x)
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE, na.rm = TRUE, type = type)
  x <- x - q[2L]
  u <- cu * pmax(q[3L] - q[2L], abs(a * q[2L]))
  l <- -cl * pmax(q[2L] - q[1L], abs(a * q[2L]))
  x > u | x < l
}

resistant_fences <- function(x, cu = 2.5, cl = cu, a = 0, type = 7) {
  x <- as.numeric(x)
  q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE, na.rm = TRUE, type = type)
  iqr <- pmax(q[3L] - q[1L], abs(a * q[2L]))
  u <- q[3L] + cu * iqr
  l <- q[1L] - cl * iqr
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
  tail <- x < q[1L] | x > q[2L]
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
