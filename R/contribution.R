contribution_geometric <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types[["geometric_index_types"]])
  geometric_to_arithmetic(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type, na.rm), na.rm) * p1 / p0
}

contribution_harmonic<- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types[["harmonic_index_types"]])
  harmonic_to_arithmetic(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type, na.rm), na.rm) * p1 / p0
}

contribution_fisher <- function (p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  Qf <- index_fisher(q1, q0, p1, p0, na.rm)
  Ql <- index_arithmetic(q1, q0, p1, p0, type = "Laspeyres", na.rm = na.rm)
  (Qf / (Qf + Ql) * index_weights(p1, p0, q1, q0, type = "Laspeyres", na.rm = na.rm) + 
    Ql / (Qf + Ql) * index_weights(p1, p0, q1, q0, type = "HybridPaasche", na.rm = na.rm)) * p1 / p0
}