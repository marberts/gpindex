index_weights <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  # check input
  check_weights_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  # match type arguments
  type <- match.arg(type, types[["weight_types"]])
  # Calculate weights
  if (length(p0) == 0L) return(numeric(0))
  out <- switch(type,
         Carli = rep_len(1, length(p0)),
         Dutot = p0,
         Laspeyres = p0 * q0,
         HybridLaspeyres = p1 * q0,
         Paasche = p1 * q1,
         HybridPaasche = p0 * q1,
         Palgrave = p1 * q1,
         Unnamed = 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1),
         Drobish = 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p0 * q1 / sum(p0 * q1),
         Walsh1 = p0 * sqrt(q0 * q1),
         MarshallEdgeworth = p0 * (q0 + q1),
         GearyKhamis = p0 / (1 / p0 + 1 / p1),
         Jevons = rep_len(1, length(p0)),
         Tornqvist = 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1),
         SatoVartia = logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)),
         Vartia2 = logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)),
         Walsh2 = sqrt(p0 * q0 * p1 * q1),
         Lowe = p0 * qb,
         Young = pb * qb,
         Coggeshall = rep_len(1, length(p0))
  ) 
  out / sum(out, na.rm = na.rm)
}
