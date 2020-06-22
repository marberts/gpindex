index_weights <- function(p1, p0, q1, q0, type, na.rm = FALSE, 
                          scale = !is.element(type, c("Vartia1", "MontgomeryVartia"))) {
  # check input
  stopifnot(
    "'p0' must be a numeric vector" = 
      missing(p0) || is.vector(p0, "numeric"),
    "'p1' must be a numeric vector" = 
      missing(p1) || is.vector(p1, "numeric"),
    "'q1' must be a numeric vector" = 
      missing(q1) || is.vector(q1, "numeric"),
    "'q0' must be a numeric vector" = 
      missing(q0) || is.vector(q0, "numeric"),
    "'p1' and 'p0' must be the same length" = 
      (missing(p1) || missing(p0)) || (length(p0) == length(p1)),
    "'q1' must be the same length as 'p1' and 'p0'" = 
      missing(q1) || ((missing(p0) || length(p0) == length(q1)) && 
                        (missing(p1) || length(p1) == length(q1))),
    "'q0' must be the same length as 'p1' and 'p0'" = 
      missing(q0) || ((missing(p0) || length(p0) == length(q0)) && 
                        (missing(p1) || length(p1) == length(q0)))
  )
  # this displays a nicer error when type is not matched
  type <- tryCatch(
    match.arg(type, types$weight_types),
    error = function(e) structure(TRUE, msg = e$message)
  )
  if (isTRUE(type)) stop(attr(type, "msg"))
  stopifnot(
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm),
    "'scale' must be TRUE or FALSE" = 
      length(scale) == 1L && is.logical(scale) && !is.na(scale)
  )
  # always return a length-0 output if inputs are length 0
  n <- if (missing(p1)) length(p0) else length(p1)
  if (n == 0L) return(numeric(0))
  # calculate weights
  out <- switch(type,
                Carli = ,
                Jevons = ,
                Coggeshall = rep.int(1, n),
                Dutot = p0,
                Young = ,
                Lowe = ,
                LloydMoulton = ,
                Laspeyres = p0 * q0,
                HybridLaspeyres = p1 * q0,
                Palgrave = ,
                Paasche = p1 * q1,
                HybridPaasche = p0 * q1,
                Drobish = 0.5 * p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 
                  0.5 * p0 * q1 / sum(p0 * q1, na.rm = TRUE),
                Unnamed = ,
                Tornqvist = 0.5 * p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 
                  0.5 * p1 * q1 / sum(p1 * q1, na.rm = TRUE),
                Walsh1 = p0 * sqrt(q0 * q1),
                Walsh2 = sqrt(p0 * q0 * p1 * q1),
                MarshallEdgeworth = p0 * (q0 + q1),
                GearyKhamis = p0 / (1 / q0 + 1 / q1),
                Vartia1 = ,
                MontgomeryVartia = logmean(p0 * q0, p1 * q1) / 
                  logmean(sum(p0 * q1, na.rm = TRUE), sum(p1 * q1, na.rm = TRUE)),
                Vartia2 = ,
                SatoVartia = logmean(p0 * q0 / sum(p0 * q0, na.rm = TRUE), 
                                     p1 * q1 / sum(p1 * q1, na.rm = TRUE))
  )
  if (scale) weights_scale(out, na.rm) else out
}