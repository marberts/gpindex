contribution_geometric <- function(p1, p0, q1, q0, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm)
  type <- match.arg(type, 
                    setdiff(types$geometric_index_types, 
                            c("Vartia1", "MontgomeryVartia"))
                    )
  w <- index_weights(p1, p0, q1, q0, type)
  weights_g2a(p1 / p0, w, na.rm) * p1 / p0
}

contribution_harmonic <- function(p1, p0, q1, q0, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm)
  type <- match.arg(type, types$harmonic_index_types)
  w <- index_weights(p1, p0, q1, q0, type)
  weights_h2a(p1 / p0, w, na.rm) * p1 / p0
}

contribution_fisher <- function(p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm)
  Qf <- index_fisher(q1, q0, p1, p0, na.rm)
  Ql <- index_arithmetic(q1, q0, p1, p0, "Laspeyres", na.rm)
  wl <- weights_scale(index_weights(p1, p0, q1, q0, "Laspeyres"), na.rm)
  wp <- weights_scale(index_weights(p1, p0, q1, q0, "HybridPaasche"), na.rm)
  (Qf / (Qf + Ql) * wl + Ql / (Qf + Ql) * wp) * p1 / p0
}
