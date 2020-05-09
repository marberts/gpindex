change_weights <- function (x, w, r, k, na.rm = FALSE, M) {
  stopifnot(
    "x must be numeric or logical" = is.numeric(x) || is.logical(x),
    "weights must be numeric or logical" = missing(w) || (is.numeric(w) || is.logical(w)),
    "r must be a length 1 numeric" = length(r) == 1L && is.numeric(r) && is.finite(r),
    "k must be length 1 numeric " = length(k) == 1L && is.numeric(k) && is.finite(k),
    "na.rm must be a length 1 numeric" = length(na.rm) == 1L && is.logical(na.rm),
    "M must be a length 1 numeric" = missing(M) || (length(M) == 1L && is.numeric(M))
  )
  # set w to 1 / n if equally weighted and calculate r-mean
  if (missing(w)) {
    if (length(x) > 0) {
      w <- 1 / length(x)
    } else {
      w <- numeric(0)
    }
    if (missing(M)) {
      M <- generalized_mean(x, r = r, na.rm = na.rm)
    }
  } else if (missing(M)) {
    M <- generalized_mean(x, w, r, na.rm = na.rm)
  }
  out <- if (r < 1 && k < 1) {
    w * generalized_logmean(x, M, k)^(1 - k) / generalized_logmean(x, M, r)^(1 - r)
  } else if (r < 1 && k >= 1) {
    if (k == 1) {
      w / generalized_logmean(x, M, r)^(1 - r)
    } else {
      w / (generalized_logmean(x, M, r)^(1 - r) * generalized_logmean(x, M, k)^(k - 1))
    }
  } else if (r >= 1 && k < 1) {
    if (r == 1) {
      w * generalized_logmean(x, M, k)^(1 - k)
    } else {
      w * generalized_logmean(x, M, r)^(r - 1) * generalized_logmean(x, M, k)^(1 - k)
    }
  } else if (r >= 1 && k >= 1) {
    if (r == 1 && k == 1) {
      w
    } else if (r == 1) {
      w / generalized_logmean(x, M, k)^(k - 1)
    } else if (k == 1){
      w * generalized_logmean(x, M, r)^(r - 1)
    } else {
      w * generalized_logmean(x, M, r)^(r - 1) / generalized_logmean(x, M, k)^(k - 1)
    }
  } 
  out / sum(out, na.rm = na.rm)
}
  
geometric_to_arithmetic <- function (x, w, na.rm = FALSE, M) change_weights(x, w, 0, 1, na.rm, M)

harmonic_to_arithmetic <- function (x, w, na.rm = FALSE, M) change_weights(x, w, -1, 1, na.rm, M)

arithmetic_to_geometric <- function (x, w, na.rm = FALSE, M) change_weights(x, w, 1, 0, na.rm, M)

harmonic_to_geometric <- function (x, w, na.rm = FALSE, M) change_weights(x, w, -1, 0, na.rm, M)


