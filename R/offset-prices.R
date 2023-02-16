offset_period <- function(f) {
  f <- match.fun(f)

  function(period, product = gl(1, length(period))) {
    if (length(period) != length(product)) {
      stop(gettext("'period' and 'product' must be the same length"))
    }
    if (length(period) == 0L) return(integer(0L))
    period <- as.factor(period)
    # factors with no levels throw an error below
    if (nlevels(period) == 0L) {
      return(rep.int(NA_integer_, length(period)))
    }
    product <- as.factor(product)
    attributes(product) <- NULL # matching is faster on factor codes
    product <- split(product, period)
    if (max(vapply(product, anyDuplicated, numeric(1L), incomparables = NA)) > 0) {
      warning(gettext("there are duplicated period-product pairs"))
    }
    m <- .mapply(match, list(product, f(product)), list(incomparables = NA))
    res <- split(seq_along(period), period)
    unsplit(.mapply(`[`, list(f(res), m), list()), period)
  }
}

back_period <- offset_period(function(x) x[c(1L, seq_len(length(x) - 1L))])

base_period <- offset_period(function(x) x[1L])
