#' Outlier detection for price relatives
#'
#' Standard cutoff-based methods for detecting outliers with price relatives.
#'
#' Each of these functions constructs an interval of the form \eqn{[b_l(x) -
#' c_l \times l(x), b_u(x) + c_u \times u(x)]}{[bl(x) - cl * l(x), bu(x) + cu *
#' u(x)]} and assigns a value in `x` as `TRUE` if that value does not
#' belong to the interval, `FALSE` otherwise. The methods differ in how
#' they construct the values \eqn{b_l(x)}{bl(x)}, \eqn{b_u(x)}{bu(x)},
#' \eqn{l(x)}, and \eqn{u(x)}. Any missing values in `x` are ignored when
#' calculating the cutoffs, but will return `NA`.
#'
#' The fixed cutoff method is the simplest, and just uses the interval
#' \eqn{[c_l, c_u]}{[cl, cu]}.
#'
#' The quartile method and Tukey algorithm are described in paragraphs 5.113 to
#' 5.135 of the CPI manual (2020), as well as by Rais (2008) and Hutton (2008).
#' The resistant fences method is an alternative to the quartile method, and is
#' described by Rais (2008) and Hutton (2008). Quantile-based methods often
#' identify price relatives as outliers because the distribution is
#' concentrated around 1; setting `a > 0` puts a floor on the minimum
#' dispersion between quantiles as a fraction of the median. See the references
#' for more details.
#'
#' The robust Z-score is the usual method to identify relatives in the
#' (asymmetric) tails of the distribution, simply replacing the mean with the
#' median, and the standard deviation with the median absolute deviation.
#'
#' These methods often assume that price relatives are symmetrically
#' distributed (if not Gaussian). As the distribution of price relatives often
#' has a long right tail, the natural logarithm can be used to transform price
#' relative before identifying outliers (sometimes under the assumption that
#' price relatives are distributed log-normal). The Hidiroglou-Berthelot
#' transformation is another approach, described in the CPI manual (par.
#' 5.124). (Sometimes the transformed price relatives are multiplied by
#' \eqn{\max(p_1, p_0)^u}{max(p1, p0)^u}, for some
#' \eqn{0 \le u \le 1}{0 <= u <= 1}, so that products with a larger price
#' get flagged as outliers (par. 5.128).)
#'
#' @param x A strictly positive numeric vector of price relatives. These can be
#' made with, e.g., [back_period()].
#' @param cu,cl A numeric vector, or something that can be coerced into one,
#' giving the upper and lower cutoffs for each element of `x`. Recycled to the
#' same length as `x`.
#' @param a A numeric vector, or something that can be coerced into one,
#' between 0 and 1 giving the scale factor for the median to establish the
#' minimum dispersion between quartiles for each element of `x`. The default
#' does not set a minimum dispersion. Recycled to the same length as `x`.
#' @param type See [quantile()].
#'
#' @returns
#' A logical vector, the same length as `x`, that is `TRUE` if the
#' corresponding element of `x` is identified as an outlier,
#' `FALSE` otherwise.
#'
#' @seealso
#' [grouped()] to make each of these functions operate on grouped data.
#'
#' [back_period()]/[base_period()] for a simple utility function to turn prices
#' in a table into price relatives.
#' 
#' The `HBmethod()` function in the \pkg{univOutl} package for the
#' Hidiroglou-Berthelot method for identifying outliers.
#'
#' @references
#' Hutton, H. (2008). Dynamic outlier detection in price index surveys.
#' *Proceedings of the Survey Methods Section: Statistical Society of Canada Annual Meeting*.
#'
#' IMF, ILO, Eurostat, UNECE, OECD, and World Bank. (2020).
#' *Consumer Price Index Manual: Concepts and Methods*.
#' International Monetary Fund.
#'
#' Rais, S. (2008). Outlier detection for the Consumer Price Index.
#' *Proceedings of the Survey Methods Section: Statistical Society of Canada Annual Meeting*.
#'
#' @examples
#' set.seed(1234)
#'
#' x <- rlnorm(10)
#'
#' fixed_cutoff(x)
#' robust_z(x)
#' quartile_method(x)
#' resistant_fences(x) # always identifies fewer outliers than above
#' tukey_algorithm(x)
#'
#' log(x)
#' hb_transform(x)
#'
#' # Works the same for grouped data
#'
#' f <- c("a", "b", "a", "a", "b", "b", "b", "a", "a", "b")
#' grouped(quartile_method)(x, group = f)
#'
#' @name outliers
#' @export
quartile_method <- function(x, cu = 2.5, cl = cu, a = 0, type = 7) {
  x <- as.numeric(x)
  cu <- as.numeric(cu)
  # It's faster to not recycle cu, cl, or a when they're length 1.
  if (length(cu) != 1L) cu <- rep_len(cu, length(x))
  cl <- as.numeric(cl)
  if (length(cl) != 1L) cl <- rep_len(cl, length(x))
  a <- as.numeric(a)
  if (length(a) != 1L) a <- rep_len(a, length(x))
  
  q <- stats::quantile(
    x, c(0.25, 0.5, 0.75),
    names = FALSE, na.rm = TRUE, type = type
  )
  x <- x - q[2L]
  u <- cu * pmax.int(q[3L] - q[2L], abs(a * q[2L]))
  l <- -cl * pmax.int(q[2L] - q[1L], abs(a * q[2L]))
  x > u | x < l
}

#' Resistant fences
#' @rdname outliers
#' @export
resistant_fences <- function(x, cu = 2.5, cl = cu, a = 0, type = 7) {
  x <- as.numeric(x)
  cu <- as.numeric(cu)
  if (length(cu) != 1L) cu <- rep_len(cu, length(x))
  cl <- as.numeric(cl)
  if (length(cl) != 1L) cl <- rep_len(cl, length(x))
  a <- as.numeric(a)
  if (length(a) != 1L) a <- rep_len(a, length(x))
  
  q <- stats::quantile(
    x, c(0.25, 0.5, 0.75),
    names = FALSE, na.rm = TRUE, type = type
  )
  iqr <- pmax.int(q[3L] - q[1L], abs(a * q[2L]))
  u <- q[3L] + cu * iqr
  l <- q[1L] - cl * iqr
  x > u | x < l
}

#' Robust z-score
#' @rdname outliers
#' @export
robust_z <- function(x, cu = 2.5, cl = cu) {
  x <- as.numeric(x)
  cu <- as.numeric(cu)
  if (length(cu) != 1L) cu <- rep_len(cu, length(x))
  cl <- as.numeric(cl)
  if (length(cl) != 1L) cl <- rep_len(cl, length(x))
  
  med <- stats::median(x, na.rm = TRUE)
  s <- stats::mad(x, na.rm = TRUE)
  x <- x - med
  u <- cu * s
  l <- -cl * s
  x > u | x < l
}

#' Fixed cutoff
#' @rdname outliers
#' @export
fixed_cutoff <- function(x, cu = 2.5, cl = 1 / cu) {
  x <- as.numeric(x)
  cu <- as.numeric(cu)
  if (length(cu) != 1L) cu <- rep_len(cu, length(x))
  cl <- as.numeric(cl)
  if (length(cl) != 1L) cl <- rep_len(cl, length(x))
  x > cu | x < cl
}

#' Tukey's algorithm
#' @rdname outliers
#' @export
tukey_algorithm <- function(x, cu = 2.5, cl = cu, type = 7) {
  x <- as.numeric(x)
  cu <- as.numeric(cu)
  if (length(cu) != 1L) cu <- rep_len(cu, length(x))
  cl <- as.numeric(cl)
  if (length(cl) != 1L) cl <- rep_len(cl, length(x))
  
  q <- stats::quantile(
    x, c(0.05, 0.95),
    names = FALSE, na.rm = TRUE, type = type
  )
  tail <- x < q[1L] | x > q[2L]
  ts <- x[x != 1 & !tail]
  if (length(ts) == 0L) {
    return(tail)
  }
  # In some versions m is the median.
  m <- mean(ts, na.rm = TRUE)
  x <- x - m
  u <- cu * (mean(ts[ts >= m], na.rm = TRUE) - m)
  l <- -cl * (m - mean(ts[ts <= m], na.rm = TRUE))
  x > u | x < l | tail
}

#' HB transform
#' @rdname outliers
#' @export
hb_transform <- function(x) {
  x <- as.numeric(x)
  med <- stats::median(x, na.rm = TRUE)
  res <- 1 - med / x
  gemed <- x >= med
  res[gemed] <- x[gemed] / med - 1
  res
}
