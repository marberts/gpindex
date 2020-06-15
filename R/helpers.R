check_mean_arguments <- function(x, w, na.rm, scale) {
  stopifnot(
    "x must be a numeric or logical vector" = 
      (is.numeric(x) || is.logical(x)) && is.vector(x),
    "weights must be numeric or logical vector" = 
      missing(w) || ((is.numeric(w) || is.logical(w)) && is.vector(w)),
    "x and w must be the same length" = 
      missing(w) || length(x) == length(w),
    "na.rm must be a length 1 logical" = 
      length(na.rm) == 1L && is.logical(na.rm),
    "scale must be a length 1 logical" = 
      length(scale) == 1L && is.logical(scale)
  )
}

check_index_arguments <- function(p1, p0, q1, q0, na.rm) {
  stopifnot(
    "p1 must be a numeric vector" = 
      is.numeric(p1) && is.vector(p1),
    "p0 must be a numeric vector" = 
      is.numeric(p0) && is.vector(p0),
    "q1 must be a numeric vector" = 
      missing(q1) || (is.numeric(q1) && is.vector(q1)),
    "q0 must be a numeric vector" = 
      missing(q0) || (is.numeric(q0) && is.vector(q0)),
    "p1 and p0 must be the same length" = 
      length(p1) == length(p0),
    "q1 must be the same length as p1 and p0" = 
      missing(q1) || length(p0) == length(q1),
    "q0 must be the same length as p1 and p0" = 
      missing(q0) || length(p0) == length(q0),
    "na.rm must be a length 1 logical" = 
      length(na.rm) == 1L && is.logical(na.rm)
  )
}

check_weights_arguments <- function(p1, p0, q1, q0, na.rm, scale) {
  stopifnot(
    "p0 must be a numeric vector" = 
      missing(p0) || (is.numeric(p0) && is.vector(p0)),
    "p1 must be a numeric vector" = 
      missing(p1) || (is.numeric(p1) && is.vector(p1)),
    "q1 must be a numeric vector" = 
      missing(q1) || (is.numeric(q1) && is.vector(q1)),
    "q0 must be a numeric vector" = 
      missing(q0) || (is.numeric(q0) && is.vector(q0)),
    "p1 and p0 must be the same length" = 
      (missing(p1) || missing(p0)) || (length(p0) == length(p1)),
    "q1 must be the same length as p1 and p0" = 
      missing(q1) || ((missing(p0) || length(p0) == length(q1)) && (missing(p1) || length(p1) == length(q1))),
    "q0 must be the same length as p1 and p0" = 
      missing(q0) || ((missing(p0) || length(p0) == length(q0)) && (missing(p1) || length(p1) == length(q0))),
    "na.rm must be a length 1 logical" = 
      length(na.rm) == 1L && is.logical(na.rm),
    "scale must be a length 1 logical" = 
      length(scale) == 1L && is.logical(scale)
  )
}