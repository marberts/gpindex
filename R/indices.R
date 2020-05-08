index_arithmetic <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
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
  arithmetic_mean(p1 / p0, index_weights(p0, p1, q0, q1, pb, qb, type, na.rm), na.rm)
}

index_geometric <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
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
  geometric_mean(p1 / p0, index_weights(p0, p1, q0, q1, pb, qb, type, na.rm), na.rm)
}

index_harmonic <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, pb, qb, type, na.rm)
  type <- match.arg(type, c("Coggeshall",
                            "Laspeyres",
                            "Paasche")
  )
  harmonic_mean(p1 / p0, index_weights(p0, p1, q0, q1, pb, qb, type, na.rm), na.rm)
}

index_fisher <- function(p0, p1, q0, q1, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, na.rm = na.rm)
  sqrt(index_arithmetic(p0, p1, q0, q1, type = "Laspeyres", na.rm = na.rm) *
         index_harmonic(p0, p1, q0, q1, type = "Paasche", na.rm = na.rm))
}

index_hlp <- function(p0, p1, q0, q1, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, na.rm = na.rm)
  2 / (1 / index_arithmetic(p0, p1, q0, q1, type = "Laspeyres", na.rm = na.rm) +
         1 / index_harmonic(p0, p1, q0, q1, type = "Paasche", na.rm = na.rm))
}

index_lm <- function(p0, p1, q0, q1, elasticity, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, na.rm = na.rm)
  generalized_mean(p1 / p0, index_weights(p0, p1, q0, q1, type = "Laspeyres", na.rm = na.rm), 1 - elasticity, na.rm)
}

index_cswd <- function(p0, p1, na.rm = FALSE) {
  check_index_arguments(p0, p1, na.rm = na.rm)
  sqrt(arithmetic_mean(p1 / p0, na.rm = na.rm) * 
         harmonic_mean(p1 / p0, na.rm = na.rm))
}

index_cswdb <- function(p0, p1, q0, q1, na.rm = FALSE) {
  check_index_arguments(p0, p1, q0, q1, na.rm = na.rm)
  sqrt(arithmetic_mean(p1 / p0, na.rm = na.rm) / 
         arithmetic_mean(q1 / q0, na.rm = na.rm) *  
         arithmetic_mean(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

index_bw <- function(p0, p1, na.rm = FALSE) {
  check_index_arguments(p0, p1, na.rm = na.rm)
  arithmetic_mean(sqrt(p1 / p0), na.rm = na.rm) * 
    index_harmonic(sqrt(p1 / p0), na.rm = na.rm)
}
