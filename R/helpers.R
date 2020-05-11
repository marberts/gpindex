check_index_arguments <- function (p1, p0, q1, q0, pb, qb, type, na.rm) {
  stopifnot(
    "p1 must be numeric" = is.numeric(p1), 
    "p0 must be numeric" = is.numeric(p0),
    "q1 must be numeric" = missing(q1) || is.numeric(q1),
    "q0 must be numeric" = missing(q0) || is.numeric(q0),
    "pb must be numeric" = missing(pb) || is.numeric(pb),
    "qb must be numeric" = missing(qb) || is.numeric(qb),
    "p1 and p0 must be the same length" = length(p1) == length(p0),
    "q1 must be the same length as p1 and p0" = missing(q1) || length(p0) == length(q1),
    "q0 must be the same length as p1 and p0" = missing(q0) || length(p0) == length(q0),
    "pb must be the same length as p1 and p0" = missing(pb) || length(p0) == length(pb),
    "qb must be the same length as p1 and p0" = missing(qb) || length(p0) == length(qb),
    "na.rm must be a length 1 logical" = length(na.rm) == 1L && is.logical(na.rm)
  )
}

check_weights_arguments <- function (p1, p0, q1, q0, pb, qb, type) {
  stopifnot(
    "p0 must be numeric" = is.numeric(p0),
    "p1 must be numeric" = missing(p1) || is.numeric(p1),
    "q1 must be numeric" = missing(q1) || is.numeric(q1),
    "q0 must be numeric" = missing(q0) || is.numeric(q0),
    "pb must be numeric" = missing(pb) || is.numeric(pb),
    "qb must be numeric" = missing(qb) || is.numeric(qb),
    "p1 and p0 must be the same length" = missing(p1) || length(p0) == length(p1),
    "q1 must be the same length as p1 and p0" = missing(q1) || length(p0) == length(q1),
    "q0 must be the same length as p1 and p0" = missing(q0) || length(p0) == length(q0),
    "pb must be the same length as p1 and p0" = missing(pb) || length(p0) == length(pb),
    "qb must be the same length as p1 and p0" = missing(qb) || length(p0) == length(qb)
  )
}