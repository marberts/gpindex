check_mean_arguments <- function(x, w, na.rm, scale) {
  stopifnot(
    "x must be numeric or logical" = 
      is.numeric(x) || is.logical(x),
    "weights must be numeric or logical" = 
      missing(w) || (is.numeric(w) || is.logical(w)),
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
    "p1 must be numeric" = 
      is.numeric(p1),
    "p0 must be numeric" = 
      is.numeric(p0),
    "q1 must be numeric" = 
      missing(q1) || is.numeric(q1),
    "q0 must be numeric" = 
      missing(q0) || is.numeric(q0),
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
    "p0 must be numeric" = 
      missing(p0) || is.numeric(p0),
    "p1 must be numeric" = 
      missing(p1) || is.numeric(p1),
    "q1 must be numeric" = 
      missing(q1) || is.numeric(q1),
    "q0 must be numeric" = 
      missing(q0) || is.numeric(q0),
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