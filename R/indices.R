arithmetic_index <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
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
  type <- match.arg(type, c("Carli", 
                            "Dutot",
                            "Laspeyres",
                            "Palgrave", 
                            "Unnamed", 
                            "Drobish", 
                            "Walsh", 
                            "MarshallEdgeworth", 
                            "GearyKhamis",
                            "Lowe",
                            "Young")
                    )
  arithmetic_mean(p1 / p0, index_weights(p0, p1, q0, q1, pb, qb, type, na.rm))
}

geometric_index <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
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
  type <- match.arg(type, c("Jevons",
                            "Laspeyres",
                            "Paasche", 
                            "Young",
                            "Tornqvist",
                            "SatoVartia")
  )
  geometric_mean(p1 / p0, index_weights(p0, p1, q0, q1, pb, qb, type, na.rm))
}

harmonic_index <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
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
  type <- match.arg(type, c("Coggeshall",
                            "Laspeyres",
                            "Paasche")
  )
  harmonic_mean(p1 / p0, index_weights(p0, p1, q0, q1, pb, qb, type, na.rm))
}

fisher_index <- function(p0, p1, q0, q1, na.rm = FALSE) {
  sqrt(arithmetic_index(p0, p1, q0, q1, type = "Laspeyres", na.rm = na.rm) *
         harmonic_index(p0, p1, q0, q1, type = "Paasche", na.rm = na.rm))
}

lloyd_moulton_index <- function(p0, p1, q0, q1, elasticity, na.rm = FALSE) {
  generalized_mean(p1 / p0, index_weights(p0, p1, q0, q1, type = "Laspeyres", na.rm = na.rm), r = 1 - elasticity, na.rm = na.rm)
}