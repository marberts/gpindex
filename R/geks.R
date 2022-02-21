geks_matrix <- function(index, p, q, product, n, nper, window, na.rm) {
  # making base prices/quantities is the slowest part of the calculation;
  # the algorithm calculates the lower-triangular part of the GEKS matrix
  # to avoid making relatives with different bases, then uses the 
  # time-reversal property of the 'index' function
  rows <- seq_len(nper)
  lt <- lapply(rows, function(i) {
    ans <- if (i < max(window - n, 2)) {
      # only the last n + 1 rows are needed for each window, 
      # so pad the top with NAs
      rep_len(NA_real_, i - 1L)
    } else {
      js <- seq(to = i - 1L, length.out = min(window, i) - 1L)
      m <- .mapply(match, list(product[js], product[i]), list(incomparables = NA))
      bp <- .mapply(`[`, list(p[i], m), list())
      bq <- .mapply(`[`, list(q[i], m), list())
      .mapply(index, list(p1 = p[js], p0 = bp, q1 = q[js], q0 = bq), list(na.rm = na.rm))
    }
    # add the diagonal at the end
    ans <- c(unlist(ans, use.names = FALSE), 1)
    front_pad <- rep_len(NA_real_, max(i - window, 0))
    back_pad <- rep_len(NA_real_, nper - length(ans) - length(front_pad))
    # matching is only done for the lower-triangular part of the matrix
    c(front_pad, ans, back_pad)
  })
  res <- do.call(rbind, lt)
  rownames(res) <- colnames(res) <- names(p)
  # exploit time-reversal
  ut <- upper.tri(res)
  res[ut] <- 1 / t(res)[ut]
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
    mat <- geks_matrix(f, p, q, product, n, nper, window, na.rm)
    rows <- seq_len(window) - 1L
    cols <- seq(window - n, window) - 1L # only the last n + 1 indexes in the window need to be kept
    res <- vector("list", nper - window + 1)
    for (i in seq_along(res)) {
      a <- apply(mat[rows + i, cols + i, drop = FALSE], 2L, geometric_mean, na.rm = na.rm)
      res[[i]] <- a[-1L] / a[-length(a)]
    }
    res
  }
}

tornqvist_geks <- geks(geometric_index("Tornqvist"))

fisher_geks <- geks(fisher_index)
