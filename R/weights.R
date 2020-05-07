index_weights <- function(p0, p1, q0, q1, pb, qb, type, na.rm = FALSE) {
  # check input
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
  # match type arguments
  type <- match.arg(type, c("Carli", 
                            "Dutot",
                            "Laspeyres",
                            "LaspeyresHybrid",
                            "Paasche",
                            "PaascheHybrid",
                            "Palgrave", 
                            "Unnamed", 
                            "Drobish", 
                            "Walsh", 
                            "MarshallEdgeworth", 
                            "GearyKhamis",
                            "Jevons",
                            "Tornqvist",
                            "SatoVartia",
                            "Lowe",
                            "Young",
                            "Coggeshall")
  )
  # Calculate weights
  if (length(p0) == 0L) return(numeric(0))
  out <- switch(type,
         Carli = rep_len(1, length(p0)),
         Dutot = p0,
         Laspeyres = p0 * q0,
         LaspeyresHybrid = p1 * q0,
         Paasche = p1 * q1,
         PaascheHybrid = p0 * q1,
         Palgrave = p1 * q1,
         Unnamed = 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1),
         Drobish = 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p0 * q1 / sum(p0 * q1),
         Walsh = p0 * sqrt(q0 * q1),
         MarshallEdgeworth = p0 * (q0 + q1),
         GearyKhamis = p0 * (1 / p0 + 1 / p1),
         Jevons = rep_len(1, length(p0)),
         Tornqvist = 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1),
         SatoVartia = logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)),
         Lowe = p0 * qb,
         Young = pb * qb,
         Coggeshall = rep_len(1, length(p0))
  ) 
  out / sum(out, na.rm = na.rm)
}
