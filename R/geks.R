#' Make the GEKS matrix
#' @noRd
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

#' GEKS index
#'
#' Calculate an inter-temporal GEKS price index over a rolling window, as
#' described in chapter 7 of Balk (2008), by Ivancic et al. (2011), and in
#' chapter 10 of the CPI manual (2020).
#'
#' @param f A [price index function][price_indexes] that uses information on both
#' base and current-period prices and quantities, and satisfies the
#' time-reversal test. Usually a Törnqvist, Fisher, or Walsh index.
#' @param p A numeric vector of prices, the same length as `q`.
#' @param q A numeric vector of quantities, the same length as `p`.
#' @param period A factor, or something that can be coerced into one, that
#' gives the corresponding time period for each element in `p` and
#' `q`. The ordering of time periods follows the levels of `period`
#' to agree with [`cut()`][cut.Date].
#' @param product A factor, or something that can be coerced into one, that
#' gives the corresponding product identifier for each element in `p` and
#' `q`.
#' @param window A positive integer giving the length of the rolling window.
#' The default is a window that encompasses all periods in `period`.
#' Non-integers are truncated towards zero.
#' @param n A positive integer giving the length of the index series for each
#' window, starting from the end of the window. For example, if there are 13
#' periods in `window`, setting `n = 1` gives the index for period 13. The
#' default gives an index for each period in `window`. Non-integers are
#' truncated towards zero.
#' @param na.rm Passed to `f` to control if missing values are removed.
#'
#' @returns
#' `geks()` returns a function:
#'
#' \preformatted{function(p, q, period, product, window = nlevels(period), n =
#'          window - 1, na.rm = FALSE){...}}
#'
#' This calculates a period-over-period GEKS index with the desired
#' index-number formula, returning a list for each window with a named-numeric
#' vector of index values.
#'
#' `tornqvist_geks()`, `fisher_geks()`, and `walsh_geks()` each return a list
#' with a named numeric vector giving the value of the respective
#' period-over-period GEKS index for each window.
#'
#' @note
#' Like [back_period()], if multiple prices
#' correspond to a period-product pair, then the back price at a point in time
#' is always the first price for that product in the previous period. Unlike a
#' bilateral index, however, duplicated period-product pairs can have more
#' subtle implications for a multilateral index.
#'
#' @seealso
#'
#' `GEKSIndex()` in the \pkg{indexNumR} package for an implementation of the
#' GEKS index with more options.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020).
#' *Consumer Price Index Manual: Theory and Practice*.
#' International Monetary Fund.
#'
#' Ivancic, L., Diewert, W. E., and Fox, K. J. (2011). Scanner data, time
#' aggregation and the construction of price indexes.
#' *Journal of Econometrics*, 161(1): 24--35.
#'
#' @examples
#' price <- 1:6
#' quantity <- 6:1
#' period <- rep(1:3, 2)
#' product <- rep(letters[1:2], each = 3)
#'
#' tornqvist_geks(price, quantity, period, product)
#'
#' tornqvist_geks(price, quantity, period, product, window = 2)
#'
#' # Missing data
#'
#' quantity[2] <- NA
#'
#' # Use all non-missing data
#'
#' fisher_geks(price, quantity, period, product, na.rm = TRUE)
#'
#' # Remove records with any missing data
#'
#' fg <- geks(balanced(fisher_index))
#' fg(price, quantity, period, product, na.rm = TRUE)
#' 
#' # Make a Jevons GEKS index
#' jevons_geks <- geks(\(p1, p0, ..., na.rm) jevons_index(p1, p0, na.rm))
#' jevons_geks(price, quantity, period, product)
#'
#' @family price-indexes
#' @export
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

    window <- as.integer(window)
    if (length(window) > 1L || window < 2L) {
      stop("'window' must be a integer greater than or equal to 2")
    }
    if (window > nper) {
      stop(
        "'window' must be less than or equal to the number of levels in",
        " 'period'"
      )
    }

    n <- as.integer(n)
    if (length(n) > 1L || n < 1L) {
      stop("'n' must be an integer greater than or equal to 1")
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

#' Tornqvist GEKS
#' @rdname geks
#' @export
tornqvist_geks <- geks(geometric_index("Tornqvist"))

#' Fisher GEKS
#' @rdname geks
#' @export
fisher_geks <- geks(fisher_index)

#' Walsh GEKS
#' @rdname geks
#' @export
walsh_geks <- geks(arithmetic_index("Walsh"))
