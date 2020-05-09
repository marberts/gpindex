index_arithmetic <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types[["arithmetic_index_types"]])
  arithmetic_mean(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type, na.rm), na.rm)
}

index_geometric <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types[["geometric_index_types"]])
  geometric_mean(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type, na.rm), na.rm)
}

index_harmonic <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types[["harmonic_index_types"]])
  harmonic_mean(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type, na.rm), na.rm)
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
  generalized_mean(p1 / p0, index_weights(p1, p0, q1, q0, type = "Laspeyres", na.rm = na.rm), 1 - elasticity, na.rm)
}

index_cswd <- function (p1, p0, na.rm = FALSE) {
  check_index_arguments(p1, p0, na.rm = na.rm)
  sqrt(arithmetic_mean(p1 / p0, na.rm = na.rm) * 
         harmonic_mean(p1 / p0, na.rm = na.rm))
}

index_cswdb <- function (p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  sqrt(arithmetic_mean(p1 / p0, na.rm = na.rm) / 
         arithmetic_mean(q1 / q0, na.rm = na.rm) *  
         arithmetic_mean(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

index_bw <- function (p1, p0, na.rm = FALSE) {
  check_index_arguments(p1, p0, na.rm = na.rm)
  arithmetic_mean(sqrt(p1 / p0), na.rm = na.rm) * 
    index_harmonic(sqrt(p1 / p0), na.rm = na.rm)
}
