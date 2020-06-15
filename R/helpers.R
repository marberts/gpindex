check_mean_arguments <- function(x, w, na.rm, scale) {
  stopifnot(
    "x must be a numeric or logical vector" = 
      is.vector(x, "numeric") || is.vector(x, "logical"),
    "weights must be numeric or logical vector" = 
      missing(w) || (is.vector(w, "numeric") || is.vector(w, "logical")),
    "x and w must be the same length" = 
      missing(w) || length(x) == length(w),
    "na.rm must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm),
    "scale must be a TRUE or FALSE" = 
      length(scale) == 1L && is.logical(scale) && !is.na(scale)
  )
}

check_index_arguments <- function(p1, p0, q1, q0, na.rm) {
  stopifnot(
    "p1 must be a numeric vector" = 
      is.vector(p1, "numeric"),
    "p0 must be a numeric vector" = 
      is.vector(p0, "numeric"),
    "q1 must be a numeric vector" = 
      missing(q1) || is.vector(q1, "numeric"),
    "q0 must be a numeric vector" = 
      missing(q0) || is.vector(q0, "numeric"),
    "p1 and p0 must be the same length" = 
      length(p1) == length(p0),
    "q1 must be the same length as p1 and p0" = 
      missing(q1) || length(p0) == length(q1),
    "q0 must be the same length as p1 and p0" = 
      missing(q0) || length(p0) == length(q0),
    "na.rm must be a TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
}

check_weights_arguments <- function(p1, p0, q1, q0, na.rm, scale) {
  stopifnot(
    "p0 must be a numeric vector" = 
      missing(p0) || is.vector(p0, "numeric"),
    "p1 must be a numeric vector" = 
      missing(p1) || is.vector(p1, "numeric"),
    "q1 must be a numeric vector" = 
      missing(q1) || is.vector(q1, "numeric"),
    "q0 must be a numeric vector" = 
      missing(q0) || is.vector(q0, "numeric"),
    "p1 and p0 must be the same length" = 
      (missing(p1) || missing(p0)) || (length(p0) == length(p1)),
    "q1 must be the same length as p1 and p0" = 
      missing(q1) || ((missing(p0) || length(p0) == length(q1)) && (missing(p1) || length(p1) == length(q1))),
    "q0 must be the same length as p1 and p0" = 
      missing(q0) || ((missing(p0) || length(p0) == length(q0)) && (missing(p1) || length(p1) == length(q0))),
    "na.rm must be a TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm),
    "scale must be a TRUE or FALSE" = 
      length(scale) == 1L && is.logical(scale) && !is.na(scale)
  )
}