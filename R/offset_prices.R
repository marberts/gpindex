#' Factory to generate back/base prices
#' @noRd
offset_period <- function(f) {
  f <- match.fun(f)

  function(period, product = gl(1, length(period)), match_first = TRUE) {
    if (length(period) != length(product)) {
      stop("'period' and 'product' must be the same length")
    }
    period <- as.factor(period)
    # factors with no levels throws an error below
    if (nlevels(period) == 0L) {
      return(rep.int(NA_integer_, length(period)))
    }
    product <- as.factor(product)
    attributes(product) <- NULL # matching is faster on factor codes
    product <- split(product, period)
    if (duplicate_products(product)) {
      warning("there are duplicated period-product pairs")
    }
    m <- .mapply(match, list(product, f(product)), list(incomparables = NA))
    if (!match_first) {
      m[[1L]][] <- NA
    }
    res <- split(seq_along(period), period)
    unsplit(.mapply(`[`, list(f(res), m), list()), period)
  }
}

#' Offset a vector prices or quantities
#'
#' For each product, compute either the position of the previous period (back
#' period), or the position of the first period (base period). Useful when price
#' information is stored in a table.
#'
#' @param period A factor, or something that can be coerced into one, that
#' gives the time period for each transaction. The ordering of time periods
#' follows the levels of `period` to agree with
#' [`cut()`][cut.Date].
#' @param product A factor, or something that can be coerced into one, that
#' gives the product identifier for each transaction. The default is to assume
#' that all transactions are for the same product.
#' @param match_first Should products in the first period match with
#' themselves (the default)?
#' 
#' @returns
#' Both functions return a numeric vector of indices for the back/base periods.
#' With `back_period()`, for all periods after the first, the resulting vector
#' gives the location of the corresponding product in the previous period.
#' With `base_period()`, the resulting vector gives the location of the
#' corresponding product in the first period. The locations are unchanged for 
#' the first time period if `match_first = TRUE`, `NA` otherwise.
#' 
#' @note
#' By definition, there must be at most one transaction for each product
#' in each time period to determine a back/base period. If multiple transactions
#' correspond to a period-product pair, then the back/base period at a point in
#' time is always the first position for that product in the previous period.
#'
#' @seealso
#' [outliers] for common methods to detect outliers for price relatives.
#'
#' `rs_pairs` in the \pkg{rsmatrix} package for making sales pairs.
#'
#' @examples
#' df <- data.frame(
#'   price = 1:6,
#'   product = factor(c("a", "b")),
#'   period = factor(c(1, 1, 2, 2, 3, 3))
#' )
#'
#' with(df, back_period(period, product))
#'
#' # Make period-over-period price relatives
#'
#' with(df, price / price[back_period(period, product)])
#'
#' # Make fixed-base price relatives
#'
#' with(df, price / price[base_period(period, product)])
#'
#' # Change the base period with relevel()
#'
#' with(df, price / price[base_period(relevel(period, "2"), product)])
#'
#' # Warning is given if the same product has multiple prices
#' # at any point in time
#'
#' with(df, back_period(period))
#'
#' @export
back_period <- offset_period(function(x) x[c(1L, seq_len(length(x) - 1L))])

#' Base period
#' @rdname back_period
#' @export
base_period <- offset_period(function(x) x[1L])
