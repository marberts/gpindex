generalized_logmean <- function(a, b, r) {
  # check input
  stopifnot(is.numeric(a), is.numeric(b), # a and b should be numeric
            length(r) == 1L, is.numeric(r), is.finite(r) # r should be a length 1 numeric
            )
  # return numeric(0) if either a or b is length 0
  if (length(a) == 0L || length(b) == 0L) return(numeric(0))
  # a and b must be the same length, so manually recycle if necessary
  if (length(a) > length(b)) {
    b <- rep_len(b, length(a))
  } else if (length(b) > length(a)) {
    a <- rep_len(a, length(b))
  }
  # calculate generalized logmean
  out <- if (r == 0) {
    # regular logmean if r = 0
    (a - b) / log(a / b)
  } else if (r == 1) {
    # r = 1
    (a^a / b^b)^(1 / (b - a)) / exp(1)
  } else {
    # general case
    if (r > 0 & r < 1) {
      (r * (a - b) / (a^r - b^r))^(1 / (1 - r))
    } else if (r < 0) {
      (r * (a - b) / (1 / a^abs(r) - 1 / b^abs(r)))^(1 / (1 - r))
    } else {
      ((a^r - b^r) / (r * (a - b)))^(1 / (r - 1))
    }
  }
  # set output to a when a = b
  loc <- which(a == b) 
  out[loc] <- a[loc]
  out
}

logmean <- function(a, b) generalized_logmean(a = a, b = b, r = 0)
