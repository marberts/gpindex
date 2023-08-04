geks_matrix <- function(index, p, q, product, n, nper, window, na.rm) {
  # making base prices/quantities is the slowest part of the calculation;
  # the algorithm calculates the lower-triangular part of the GEKS matrix
  # to avoid making relatives with different bases, then uses the
  # time-reversal property of the 'index' function
  rows <- seq_len(nper)
  lt <- lapply(rows, function(i) {
    if (i < max(window - n, 2L)) {
      # only the last n + 1 rows are needed for each window,
      # so pad the top rows left of the diagonal with NA
      ans <- rep_len(NA_real_, i - 1L)
    } else {
      # matching is only done for the lower-triangular part of the matrix
      # match products for window - 1 periods left of the diagonal
      # to minimize the number of back prices to find
      js <- seq.int(to = i - 1L, length.out = min(window, i) - 1L)
      m <- .mapply(
        match,
        list(product[js], product[i]),
        list(incomparables = NA)
      )
      bp <- .mapply(`[`, list(p[i], m), list())
      bq <- .mapply(`[`, list(q[i], m), list())
      ans <- .mapply(
        index,
        list(p1 = p[js], p0 = bp, q1 = q[js], q0 = bq),
        list(na.rm = na.rm)
      )
    }
    # add the diagonal at the end and pad with NAs
    ans <- c(unlist(ans, use.names = FALSE), 1)
    front_pad <- rep_len(NA_real_, max(i - window, 0L))
    back_pad <- rep_len(NA_real_, nper - length(ans) - length(front_pad))
    c(front_pad, ans, back_pad)
  })
  res <- do.call(rbind, lt)
  rownames(res) <- colnames(res) <- names(p) # time periods
  # exploit time reversal
  ut <- upper.tri(res)
  res[ut] <- 1 / t(res)[ut]
  res
}

geks <- function(f) {
  f <- match.fun(f)

  function(p, q, period, product,
           window = nlevels(period), n = window - 1L, na.rm = FALSE) {
    if (different_lengths(p, q, period, product)) {
      stop("'p', 'q', 'period', and 'product' must be the same length")
    }

    period <- as.factor(period)
    nper <- nlevels(period)

    if (nper == 0L) {
      return(list())
    }

    window <- as.integer(window[1L])
    if (window < 2L) {
      stop("'window' must be greater than or equal to 2")
    }
    if (window > nper) {
      stop("'window' must be less than or equal to the number of levels in",
           " 'period'")
    }

    n <- as.integer(n[1L])
    if (n < 1L) {
      stop("'n' must be greater than or equal to 1")
    }
    if (n > window - 1L) {
      stop("'n' must be less than or equal to 'window' minus 1")
    }

    p <- split(p, period)
    q <- split(q, period)
    product <- as.factor(product)
    attributes(product) <- NULL # faster to match on numeric codes
    product <- split(product, period)
    if (duplicate_products(product)) {
      warning("there are duplicated period-product pairs")
    }

    mat <- geks_matrix(f, p, q, product, n, nper, window, na.rm)
    rows <- seq_len(window) - 1L
    # only the last n + 1 indexes in each window need to be kept
    cols <- seq.int(window - n, window) - 1L
    res <- vector("list", nper - window + 1L)
    # move down the diagonal to make the geks index
    for (i in seq_along(res)) {
      index <- apply(
        mat[rows + i, cols + i, drop = FALSE], 2L,
        geometric_mean,
        na.rm = na.rm
      )
      res[[i]] <- index[-1L] / index[-length(index)]
    }
    res
  }
}

tornqvist_geks <- geks(geometric_index("Tornqvist"))

fisher_geks <- geks(fisher_index)
