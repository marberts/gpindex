#---- Price utilities----
offset_price <- function(type = c("back", "base")) {
  type <- match.arg(type)
  # return function
  function(x, period, product = gl(1, length(x))) {
    if (!same_length(x, period, product)) {
      stop("all arguments must be the same length")
    }
    if (!length(x)) return(x[0])
    offset <- function(x) x[c(1L, if (type == "back") seq_len(length(x) - 1))]
    period <- as.factor(period)
    price <- split(x, period)
    product <- split(as.integer(as.factor(product)), period)
    if (any(vapply(product, anyDuplicated, numeric(1)) > 0)) {
      warning("there are duplicated period-product pairs") 
    }
    matches <- Map(match, product, offset(product), incomparables = NA)
    res <- unsplit(Map(`[`, offset(price), matches), period)
    attributes(res) <- attributes(x) # unsplit mangles attributes
    res
  }
}

back_price <- offset_price("back")

base_price <- offset_price("base")

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
  if (any(x <= 0, na.rm = TRUE)) {
    warning("some elements of 'x' are less than or equal to 0; the Hidiroglou-Berthelot transformation is not defined")
  }
  med <- median(x, na.rm = TRUE)
  res <- 1 - med / x
  gemed <- x >= med
  res[gemed] <- x[gemed] / med - 1
  res
}