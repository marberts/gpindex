contribution_geometric <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types$geometric_index_types)
  w <- weights_g2a(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type), na.rm)
  w / sum(w) * p1 / p0
}

contribution_harmonic <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types$harmonic_index_types)
  w <- weights_h2a(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, type), na.rm) 
  w / sum(w) * p1 / p0
}

contribution_fisher <- function (p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  Qf <- index_fisher(q1, q0, p1, p0, na.rm)
  Ql <- index_arithmetic(q1, q0, p1, p0, type = "Laspeyres", na.rm = na.rm)
  wl <- index_weights(p1, p0, q1, q0, type = "Laspeyres")
  wp <- index_weights(p1, p0, q1, q0, type = "HybridPaasche")
  (Qf / (Qf + Ql) * wl / sum(wl) + Ql / (Qf + Ql) * wp / sum(wp)) * p1 / p0
}
