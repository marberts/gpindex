#' Splice an index series
#'
#' Splice a collection of index series computed over a rolling window into one
#' index series. Splicing on multiple points combines the results with a
#' geometric mean.
#'
#' @param x A list of equal-length numeric vectors giving the period-over-period
#'   indexes for each window.
#' @param periods An integer vector giving the splice points for each window.
#'   The default splices on each point in the window.
#' @param initial A numeric vector giving an initial period-over-period index
#'   series onto which the elements of `x` are spliced. The default uses the
#'   first element of `x`.
#' @param published Should the splice be done against the published series? The
#'   default splices using the recalculated index series.
#'
#' @returns
#' A numeric vector giving the spliced (fixed-base) index series.
#'
#' @references
#' Chessa, A. G. (2019).
#' *A Comparison of Index Extension Methods for Multilateral Methods.* Paper
#' presented at the 16th Meeting of the Ottawa Group on Price Indices,
#' 8-10 May 2019, Rio de Janeiro, Brazil.
#'
#' Krsinich, F. (2016). The FEWS index: Fixed effects with a window splice.
#' *Journal of Official Statistics*, 32(2), 375-404.
#'
#' @examples
#' # Make an index series over a rolling window.
#'
#' x <- list(c(1.1, 0.9, 1.2), c(0.8, 1.3, 1.4), c(1.3, 1.3, 0.8))
#'
#' # Mean splice.
#'
#' splice_index(x)
#'
#' # Movement splice.
#'
#' splice_index(x, 3)
#'
#' # Window splice.
#'
#' splice_index(x, 1)
#'
#' # Splicing on the published series preserves the within-window
#' # movement of the index series.
#'
#' splice_index(x, 1, published = TRUE)
#'
#' @family price index functions
#' @export
splice_index <- function(x, periods = NULL, initial = NULL, published = FALSE) {
  x <- as.list(x)
  if (do.call(different_lengths, x)) {
    stop("all elements of 'x' must be the same length")
  }

  if (is.null(initial) && length(x) > 0L) {
    initial <- x[[1L]]
    x <- x[-1L]
  }
  initial <- cumprod(initial)
  if (length(x) == 0L) {
    return(initial)
  }

  n <- length(x[[1L]])
  offset <- length(initial)
  if (offset < n) {
    stop("'initial' must be at least as long as each element of 'x'")
  }

  if (is.null(periods)) {
    periods <- seq_len(n)
  } else {
    periods <- as.integer(periods)
  }

  y <- lapply(x, \(z) rev(cumprod(rev(z)))[periods])
  # Use recursive = TRUE to keep names.
  res <- c(initial, lapply(x, \(x) x[length(x)]), recursive = TRUE)

  iw <- seq.int(to = offset - 1L, length.out = n)[periods]
  if (published) {
    for (i in seq_along(x)) {
      res[i + offset] <- geometric_mean(y[[i]] * res[iw + i])
    }
  } else {
    links <- c(
      list(rep.int(1, length(periods))),
      lapply(x[-length(x)], `[`, periods)
    )
    links <- Reduce(`*`, links, accumulate = TRUE)
    base <- res[iw + 1L]
    for (i in seq_along(x)) {
      res[i + offset] <- geometric_mean(y[[i]] * links[[i]] * base)
    }
  }
  res
}
