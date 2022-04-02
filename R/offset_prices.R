offset_period <- function(f) {
  f <- match.fun(f)
  # return function
  function(period, product = gl(1, length(period))) {
    if (length(period) != length(product)) {
      stop(gettext("'period' and 'product' must be the same length"))
    }
    if (!length(period)) return(integer(0L))
    period <- as.factor(period)
    product <- as.factor(product)
    attributes(product) <- NULL # matching is faster on factor codes
    product <- split(product, period)
    if (max(vapply(product, anyDuplicated, numeric(1L), incomparables = NA))) {
      warning(gettext("there are duplicated period-product pairs"))
    }
    m <- .mapply(match, list(product, f(product)), list(incomparables = NA))
    res <- split(seq_along(period), period)
    unsplit(.mapply(`[`, list(f(res), m), list()), period)
  }
}

back_period <- offset_period(function(x) x[c(1L, seq_len(length(x) - 1L))])

base_period <- offset_period(function(x) x[1L])

# TODO: deprecated, to be removed
offset_price <- function(f) {
  f <- match.fun(f)
  # return function
  function(x, period, product = gl(1, length(x))) {
    warning("back_price()/base_price() are deprecated; use back_period()/base_period() instead")
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
