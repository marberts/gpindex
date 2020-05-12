index_weights <- function (p1, p0, q1, q0, pb, qb, type) {
  # check input
  check_weights_arguments(p1, p0, q1, q0, pb, qb, type)
  # match type arguments
  type <- match.arg(type, types$weight_types)
  # Calculate weights
  if (length(p0) == 0L) return(numeric(0))
  switch(type,
         Carli = ,
         Jevons = ,
         Coggeshall = rep.int(1, length(p0)), 
         Dutot = p0,
         Laspeyres = p0 * q0,
         HybridLaspeyres = p1 * q0,
         Palgrave = ,
         Paasche = p1 * q1,
         HybridPaasche = p0 * q1,
         Drobish = 0.5 * p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 0.5 * p0 * q1 / sum(p0 * q1, na.rm = TRUE),
         Unnamed = ,
         Tornqvist = 0.5 * p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 0.5 * p1 * q1 / sum(p1 * q1, na.rm = TRUE),
         Walsh1 = p0 * sqrt(q0 * q1),
         Walsh2 = sqrt(p0 * q0 * p1 * q1),
         MarshallEdgeworth = p0 * (q0 + q1),
         GearyKhamis = p0 / (1 / p0 + 1 / p1),
         Vartia1 = ,
         MontgomeryVartia = logmean(p0 * q0, p1 * q1) / logmean(sum(p0 * q1, na.rm = TRUE), sum(p1 * q1, na.rm = TRUE)),
         Vartia2 = ,
         SatoVartia = logmean(p0 * q0 / sum(p0 * q0, na.rm = TRUE), p1 * q1 / sum(p1 * q1, na.rm = TRUE)),
         Lowe = p0 * qb,
         Young = pb * qb,
  ) 
}

# index_weights <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
#   # check input
#   check_weights_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
#   # match type arguments
#   type <- match.arg(type, types$weight_types)
#   # Calculate weights
#   if (length(p0) == 0L) return(numeric(0))
#   if (type %in% types$value_weights) {
#     if (anyNA(list(p1, p0, q0, q1), TRUE)) {
#       if (na.rm) {
#         comp <- stats::complete.cases(p1, p0, q1, q0)
#         v0 <- sum((p0 * q0)[comp])
#         v1 <- if (type == "Drobish") sum((p0 * q1)[comp]) else sum((p1 * q1)[comp])
#       } else {
#         return(rep.int(NA_real_, length(p0)))
#       }
#     } else {
#       v0 <- sum(p0 * q0)
#       v1 <- if (type == "Drobish") sum(p0 * q1) else sum(p1 * q1)
#     }
#     switch(type,
#            Drobish = 0.5 * p0 * q0 / v0 + 0.5 * p0 * q1 / v1,
#            Unnamed = ,
#            Tornqvist = 0.5 * p0 * q0 / v0 + 0.5 * p1 * q1 / v1,
#            Vartia1 = ,
#            MontgomeryVartia = logmean(p0 * q0, p1 * q1) / logmean(v0, v1),
#            Vartia2 = ,
#            SatoVartia = logmean(p0 * q0 / v0, p1 * q1 / v1),
#     )
#   } else {
#     switch(type,
#            Carli = ,
#            Jevons = ,
#            Coggeshall = rep.int(1, length(p0)), 
#            Dutot = p0,
#            Laspeyres = p0 * q0,
#            HybridLaspeyres = p1 * q0,
#            Palgrave = ,
#            Paasche = p1 * q1,
#            HybridPaasche = p0 * q1,
#            Walsh1 = p0 * sqrt(q0 * q1),
#            Walsh2 = sqrt(p0 * q0 * p1 * q1),
#            MarshallEdgeworth = p0 * (q0 + q1),
#            GearyKhamis = p0 / (1 / p0 + 1 / p1),
#            Lowe = p0 * qb,
#            Young = pb * qb
#     ) 
#   }
# }

