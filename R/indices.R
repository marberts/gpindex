arithmetic_index <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, pb, qb, type, na.rm)
  type <- match.arg(type, c("Carli", 
                            "Dutot",
                            "Laspeyres",
                            "Palgrave", 
                            "Unnamed", 
                            "Drobish", 
                            "Walsh1", 
                            "MarshallEdgeworth", 
                            "GearyKhamis",
                            "Lowe",
                            "Young")
                    )
  arithmetic_mean(p1 / p0, index_weights(p0, p1, q0, q1, pb, qb, type, na.rm))
}

geometric_index <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, pb, qb, type, na.rm)
  type <- match.arg(type, c("Jevons",
                            "Laspeyres",
                            "Paasche", 
                            "Young",
                            "Tornqvist",
                            "SatoVartia",
                            "Vartia2",
                            "Walsh2")
  )
  geometric_mean(p1 / p0, index_weights(p0, p1, q0, q1, pb, qb, type, na.rm))
}

harmonic_index <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, pb, qb, type, na.rm)
  type <- match.arg(type, c("Coggeshall",
                            "Laspeyres",
                            "Paasche")
  )
  harmonic_mean(p1 / p0, index_weights(p0, p1, q0, q1, pb, qb, type, na.rm))
}

fisher_index <- function(p0, p1, q0, q1, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, na.rm = na.rm)
  sqrt(arithmetic_index(p0, p1, q0, q1, type = "Laspeyres", na.rm = na.rm) *
         harmonic_index(p0, p1, q0, q1, type = "Paasche", na.rm = na.rm))
}

hlp_index <- function(p0, p1, q0, q1, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, na.rm = na.rm)
  2 / (1 / arithmetic_index(p0, p1, q0, q1, type = "Laspeyres", na.rm = na.rm) +
         1 / harmonic_index(p0, p1, q0, q1, type = "Paasche", na.rm = na.rm))
}

lloyd_moulton_index <- function(p0, p1, q0, q1, elasticity, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, na.rm = na.rm)
  generalized_mean(p1 / p0, index_weights(p0, p1, q0, q1, type = "Laspeyres", na.rm = na.rm), r = 1 - elasticity, na.rm = na.rm)
}