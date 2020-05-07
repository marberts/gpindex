check_index_arguments <- function(p0, p1, q0, q1, pb, qb, type, na.rm) {
  stopifnot(is.numeric(p0), is.numeric(p1),
            missing(q0) || is.numeric(q0),
            missing(q1) || is.numeric(q1),
            missing(pb) || is.numeric(pb),
            missing(qb) || is.numeric(qb),
            length(p0) == length(p1),
            missing(q0) || length(p0) == length(q0),
            missing(q1) || length(p0) == length(q1),
            missing(pb) || length(p0) == length(pb),
            missing(qb) || length(p0) == length(qb),
            length(na.rm) == 1L, is.logical(na.rm)
  )
}

check_weights_arguments <- function(p0, p1, q0, q1, pb, qb, type, na.rm) {
  stopifnot(is.numeric(p0),
            missing(p1) || is.numeric(p1),
            missing(q0) || is.numeric(q0),
            missing(q1) || is.numeric(q1),
            missing(pb) || is.numeric(pb),
            missing(qb) || is.numeric(qb),
            missing(p1) || length(p0) == length(p1),
            missing(q0) || length(p0) == length(q0),
            missing(q1) || length(p0) == length(q1),
            missing(pb) || length(p0) == length(pb),
            missing(qb) || length(p0) == length(qb),
            length(na.rm) == 1L, is.logical(na.rm)
  )
}