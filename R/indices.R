index_arithmetic <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types$arithmetic_index_types)
  mean_arithmetic(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type), na.rm)
}

index_geometric <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types$geometric_index_types)
  mean_geometric(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type), na.rm, scale = !type %in% c("Vartia1", "Montgomery-Vartia"))
}

index_harmonic <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types$harmonic_index_types)
  mean_harmonic(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type), na.rm)
}

index_fisher <- function (p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  sqrt(index_arithmetic(p1, p0, q1, q0, type = "Laspeyres", na.rm = na.rm) *
         index_harmonic(p1, p0, q1, q0, type = "Paasche", na.rm = na.rm))
}

index_hlp <- function (p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  2 / (1 / index_arithmetic(p1, p0, q1, q0, type = "Laspeyres", na.rm = na.rm) +
         1 / index_harmonic(p1, p0, q1, q0, type = "Paasche", na.rm = na.rm))
}

index_lm <- function (p1, p0, q1, q0, elasticity, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  mean_generalized(p1 / p0, index_weights(p1, p0, q1, q0, type = "Laspeyres"), 1 - elasticity, na.rm)
}

index_cswd <- function (p1, p0, na.rm = FALSE) {
  check_index_arguments(p1, p0, na.rm = na.rm)
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) * 
         mean_harmonic(p1 / p0, na.rm = na.rm))
}

index_cswdb <- function (p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) / 
         mean_arithmetic(q1 / q0, na.rm = na.rm) *  
         mean_arithmetic(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

index_bw <- function (p1, p0, na.rm = FALSE) {
  check_index_arguments(p1, p0, na.rm = na.rm)
  mean_arithmetic(sqrt(p1 / p0), na.rm = na.rm) * 
    index_harmonic(sqrt(p1 / p0), na.rm = na.rm)
}
