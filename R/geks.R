#' GEKS index
#'
#' Calculate a generalized inter-temporal GEKS price index over a rolling
#' window.
#'
#' @param f A [price index function][price_indexes] that uses information on
#'   both base and current-period prices and quantities, and satisfies the
#'   time-reversal test. Usually a Törnqvist, Fisher, or Walsh index.
#' @param r A finite number giving the order of the generalized mean used to
#'   average price indexes over the rolling window. The default uses a
#'   geometric mean.
#' @param p A numeric vector of prices, the same length as `q`.
#' @param q A numeric vector of quantities, the same length as `p`.
#' @param period A factor, or something that can be coerced into one, that
#'   gives the corresponding time period for each element in `p` and
#'   `q`. The ordering of time periods follows the levels of `period`
#'   to agree with [`cut()`][cut.Date].
#' @param product A factor, or something that can be coerced into one, that
#'   gives the corresponding product identifier for each element in `p` and
#'   `q`.
#' @param window A positive integer giving the length of the rolling window.
#'   The default is a window that encompasses all periods in `period`.
#'   Non-integers are truncated towards zero.
#' @param n A positive integer giving the length of the index series for each
#'   window, starting from the end of the window. For example, if there are 13
#'   periods in `window`, setting `n = 1` gives the index for period 13. The
#'   default gives an index for each period in `window`. Non-integers are
#'   truncated towards zero.
#' @param na.rm Passed to `f` to control if missing values are removed.
#' @param match_method Either 'all' to match all products against each other
#'   (the default) or 'back-price' to match only back prices. The later can be
#'   faster when there is lots of product imbalanced, but should be used with
#'   a balanced index-number formula `f`.
#'
#' @returns
#' `geks()` returns a function:
#'
#' \preformatted{function(p,
#'          q,
#'          period,
#'          product,
#'          window = nlevels(period),
#'          n = window - 1,
#'          na.rm = FALSE,
#'          match_method = c("all", "back-price")){...}}
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
#' `GEKSIndex()` in the \pkg{IndexNumR} package for an implementation of the
#' GEKS index with more options.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers*.
#' Cambridge University Press.
#'
#' IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020).
#' *Consumer Price Index Manual: Concepts and Methods*.
#' International Monetary Fund.
#'
#' Ivancic, L., Diewert, W. E., and Fox, K. J. (2011). Scanner data, time
#' aggregation and the construction of price indexes.
#' *Journal of Econometrics*, 161(1): 24--35.
#'
#' @examples
#' price <- 1:10
#' quantity <- 10:1
#' period <- rep(1:5, 2)
#' product <- rep(letters[1:2], each = 5)
#'
#' cumprod(tornqvist_geks(price, quantity, period, product)[[1]])
#'
#' # Calculate the index over a rolling window.
#'
#' (tg <- tornqvist_geks(price, quantity, period, product, window = 3))
#'
#' # Use a movement splice to combine the indexes in each window.
#'
#' splice_index(tg, 2)
#'
#' # ... or use a mean splice.
#'
#' splice_index(tg)
#'
#' # Use all non-missing data.
#'
#' quantity[2] <- NA
#' fisher_geks(price, quantity, period, product, na.rm = TRUE)
#'
#' # Remove records with any missing data.
#'
#' fg <- geks(balanced(fisher_index))
#' fg(price, quantity, period, product, na.rm = TRUE)
#'
#' # Make a Jevons GEKS index.
#'
#' jevons_geks <- geks(\(p1, p0, ..., na.rm) jevons_index(p1, p0, na.rm))
#' jevons_geks(price, quantity, period, product)
#'
#' @family price index functions
#' @export
geks <- function(f, r = 0) {
  f <- match.fun(f)
  gen_mean <- generalized_mean(r)
  function(p,
           q,
           period,
           product,
           window = nlevels(period),
           n = window - 1L,
           na.rm = FALSE,
           match_method = c("all", "back-price")) {
    period <- as.factor(period)
    product <- as.factor(product)
    attributes(product) <- NULL # faster to match on numeric codes

    if (different_lengths(p, q, period, product)) {
      stop("'p', 'q', 'period', and 'product' must be the same length")
    }

    match_method <- match.arg(match_method)

    if (nlevels(period) == 0L) {
      return(list())
    }

    window <- as.integer(window)
    if (length(window) > 1L || window < 2L) {
      stop("'window' must be a integer greater than or equal to 2")
    }
    if (window > nlevels(period)) {
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

    mat <- geks_matrix(f, p, q, period, product, window, n, na.rm, match_method)
    rows <- seq_len(window) - 1L
    # Only the last n + 1 indexes in each window need to be kept.
    cols <- seq.int(window - n, window) - 1L
    res <- vector("list", nlevels(period) - window + 1L)
    # Move down the diagonal to make the geks index.
    for (i in seq_along(res)) {
      index <- apply(
        mat[rows + i, cols + i, drop = FALSE],
        2L,
        gen_mean,
        na.rm = na.rm
      )
      res[[i]] <- index[-1L] / index[-length(index)]
    }
    res
  }
}

#' Make the GEKS matrix
#' @noRd
geks_matrix <- function(index, p, q, period, product, window, n, na.rm, method) {
  p <- split(p, period)
  q <- split(q, period)

  if (method == "all") {
    product <- balance_products(product, period)
    p <- Map(`[`, p, product)
    q <- Map(`[`, q, product)
  } else {
    product <- split(product, period)
  }

  lt <- vector("list", nlevels(period))
  for (i in seq_along(lt)) {
    if (i < max(window - n, 2L)) {
      # Only the last n + 1 rows are needed for each window,
      # so pad the top rows left of the diagonal with NA.
      ans <- rep_len(NA_real_, i - 1L)
    } else {
      # Matching is only done for the lower-triangular part of the matrix.
      # Match products for window - 1 periods left of the diagonal
      # to minimize the number of back prices to find.
      js <- seq.int(to = i - 1L, length.out = min(window, i) - 1L)
      if (method == "all") {
        ans <- .mapply(
          index,
          list(p1 = p[js], p0 = p[i], q1 = q[js], q0 = q[i]),
          list(na.rm = na.rm)
        )
      } else {
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
    }
    # Add the diagonal at the end (fixing #8) and pad with NAs.
    ans <- c(
      unlist(ans, use.names = FALSE),
      index(p[[i]], p[[i]], q[[i]], q[[i]], na.rm = TRUE)
    )
    front_pad <- rep_len(NA_real_, max(i - window, 0L))
    back_pad <- rep_len(NA_real_, length(lt) - length(ans) - length(front_pad))
    lt[[i]] <- c(front_pad, ans, back_pad)
  }
  res <- do.call(rbind, lt)
  rownames(res) <- colnames(res) <- names(p) # time periods
  # Exploit time reversal.
  ut <- upper.tri(res)
  res[ut] <- 1 / t(res)[ut]
  res
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
