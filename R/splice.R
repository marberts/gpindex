#' Splice an index series
#' 
#' Splice a collection of index series computed over a rolling window into one
#' index series. Splicing on multiple points combines the results with a
#' geometric mean.
#' 
#' @param x A list of equal-length numeric vectors giving the period-over-period
#' indexes for each window.
#' @param periods A vector (usually numeric) used to subscript each element of 
#' `x` and give the splice points for each window. The default splices on each
#' point in the window.
#' @param initial A numeric vector giving an initial period-over-period index
#' series onto which the elements of `x` are spliced. The default uses the
#' first element of `x`.
#' 
#' @returns
#' A numeric vector giving the spliced (fixed-base) index series.
#' 
#' @examples
#' # Make an index series over a rolling window
#' x <- list(c(1.1, 0.9, 1.2), c(0.8, 1.3, 1.4), c(1.3, 1.3, 0.8))
#' 
#' # Mean splice
#' 
#' splice_index(x)
#' 
#' # Movement splice
#' 
#' splice_index(x, 3)
#' 
#' # Window splice
#' 
#' splice_index(x, 1)
#' 
#' @family price-indexes
#' @export
splice_index <- function(x, periods = NULL, initial = NULL) {
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
  
  offset <- length(initial)
  if (offset < length(x[[1L]])) {
    stop("'initial' must be at least as long as each element of 'x'")
  }
  
  if (is.null(periods)) {
    periods <- seq_along(x[[1L]])
  }
  
  x <- lapply(x, \(z) rev(cumprod(rev(z))))
  res <- numeric(offset + length(x))
  res[seq_along(initial)] <- initial
  for (i in seq_along(x)) {
    window <- x[[i]]
    iw <- seq.int(to = i + offset - 1L, length.out = length(window))
    res[i + offset] <- geometric_mean(window[periods] * res[iw[periods]])
  }
  res
}