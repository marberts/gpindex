geks_matrix <- function(index, p, q, product, n, nper, na.rm) {
  # making base prices/quantities is the slowest part of the calculation;
  # the algorithm calculates the lower-triangular part of the GEKS matrix
  # to avoid making relatives with different bases, then uses the 
  # time-reversal property of the 'index' function
  rows <- seq_len(nper)
  lt <- lapply(rows, function(i) {
    # only the last n + 1 rows are needed, so pad the top with 1s
    if (i < max(nper - n, 2)) return(rep_len(1, nper))
    js <- seq_len(i - 1)
    pad <- rep_len(1, nper - i + 1)
    # matching is only done for the lower-triangular part of the matrix
    m <- .mapply(match, list(product[js], product[i]), list(incomparables = NA))
    bp <- .mapply(`[`, list(p[i], m), list())
    bq <- .mapply(`[`, list(q[i], m), list())
    ans <- .mapply(index, list(p1 = p[js], p0 = bp, q1 = q[js], q0 = bq), list(na.rm = na.rm))
    c(unlist(ans, use.names = FALSE), pad)
  })
  res <- do.call(rbind, lt)
  rownames(res) <- colnames(res) <- names(p)
  # exploit time-reversal
  res[upper.tri(res)] <- 1 / t(res)[upper.tri(res)]
  res
}

geks <- function(f) {
  f <- match.fun(f)
  # return function
  function(p, q, period, product, window = nlevels(period), n = window - 1, na.rm = FALSE) {
    if (different_lengths(p, q, period, product)) {
      stop(gettext("'p', 'q', 'period', and 'product' must be the same length"))
    }
    period <- as.factor(period)
    nper <- nlevels(period)
    if (!nper) return(list())
    window <- to_scalar(window)
    if (window < 2) {
      stop(gettext("'window' must be greater than or equal to 2"))
    }
    if (window > nper) {
      stop(gettext("'window' must be less than or equal to the number of levels in 'period'"))
    }
    n <- to_scalar(n)
    if (n < 1) {
      stop(gettext("'n' must be greater than or equal to 1"))
    }
    if (n > window - 1) {
      stop(gettext("'n' must be less than or equal to 'window' minus 1"))
    }
    p <- split(p, period)
    q <- split(q, period)
    product <- as.factor(product)
    attributes(product) <- NULL # faster to match on integer codes
    product <- split(product, period)
    windows <- rolling_window(nper, window)
    keep <- seq(window - n, window) # only the last n + 1 indexes in the window need to be kept
    res <- vector("list", length(windows))
    for (i in seq_along(res)) {
      w <- windows[[i]]
      mat <- geks_matrix(f, p[w], q[w], product[w], n, window, na.rm)
      mat <- apply(mat[, keep, drop = FALSE], 2L, geometric_mean, na.rm = na.rm)
      res[[i]] <- mat[-1L] / mat[-length(mat)]
    }
    res
  }
}

tornqvist_geks <- geks(geometric_index("Tornqvist"))

fisher_geks <- geks(fisher_index)
