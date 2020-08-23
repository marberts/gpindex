index_weights <- function(type = c("Carli", "Jevons", "Coggeshall",
                                   "Dutot", "Laspeyres", "HybridLaspeyres",
                                   "Palgrave", "Paasche", "HybridPaasche",
                                   "Drobish", "Unnamed", "Tornqvist",
                                   "Walsh1", "Walsh2", "MarshallEdgeworth",
                                   "GearyKhamis", "Vartia1", "MontgomeryVartia",
                                   "Vartia2", "SatoVartia", "Lowe",
                                   "Young", "LloydMoulton")) {
  res <- switch(match.arg(type),
                Carli = ,
                Jevons = ,
                Coggeshall = function(p0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- rep(1, length(p0))
                },
                Dutot = function(p0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p0' must be a numeric vector" = 
                              is_numeric(p0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- p0
                },
                Young = ,
                Lowe = ,
                LloydMoulton = ,
                Laspeyres = function(p0, q0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p0' and 'q0' must be numeric vectors" = 
                              is_numeric(p0, q0),
                            "'p0' and 'q0' must be the same length" = 
                              same_length(p0, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- p0 * q0
                },
                HybridLaspeyres = function(p1, q0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p1' and 'q0' must be numeric vectors" = 
                              is_numeric(p1, q0),
                            "'p1' and 'q0' must be the same length" = 
                              same_length(p1, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- p1 * q0
                },
                Palgrave = ,
                Paasche = function(p1, q1, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p1' and 'q1' must be numeric vectors" = 
                              is_numeric(p1, q1),
                            "'p1' and 'q1' must be the same length" = 
                              same_length(p1, q1),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- p1 * q1
                },
                HybridPaasche = function(p0, q1, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p0' and 'q1' must be numeric vectors" = 
                              is_numeric(p0, q1),
                            "'p0' and 'q1' must be the same length" = 
                              same_length(p0, q1),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- p0 * q1
                },
                Drobish = function(p1, p0, q1, q0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
                              is_numeric(p1, p0, q1, q0),
                            "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
                              same_length(p1, p0, q1, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- (p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 
                            p0 * q1 / sum(p0 * q1, na.rm = TRUE)) / 2
                },
                Unnamed = ,
                Tornqvist = function(p1, p0, q1, q0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
                              is_numeric(p1, p0, q1, q0),
                            "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
                              same_length(p1, p0, q1, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- (p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 
                            p1 * q1 / sum(p1 * q1, na.rm = TRUE)) / 2
                },
                Walsh1 = function(p0, q1, q0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p0', 'q1', and 'q0' must be numeric vectors" = 
                              is_numeric(p0, q1, q0),
                            "'p0', 'q1', and 'q0' must be the same length" = 
                              same_length(p0, q1, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- p0 * sqrt(q0 * q1)
                },
                Walsh2 = function(p1, p0, q1, q0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
                              is_numeric(p1, p0, q1, q0),
                            "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
                              same_length(p1, p0, q1, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- sqrt(p0 * q0 * p1 * q1)
                },
                MarshallEdgeworth = function(p0, q1, q0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p0', 'q1', and 'q0' must be numeric vectors" = 
                              is_numeric(p0, q1, q0),
                            "'p0', 'q1', and 'q0' must be the same length" = 
                              same_length(p0, q1, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- p0 * (q0 + q1)
                },
                GearyKhamis = function(p0, q1, q0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p0', 'q1', and 'q0' must be numeric vectors" = 
                              is_numeric(p0, q1, q0),
                            "'p1', 'q1', and 'q0' must be the same length" = 
                              same_length(p0, q1, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- p0 / (1 / q0 + 1 / q1)
                },
                Vartia1 = ,
                MontgomeryVartia = function(p1, p0, q1, q0, na.rm = FALSE, scale = FALSE) {
                  stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
                              is_numeric(p1, p0, q1, q0),
                            "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
                              same_length(p1, p0, q1, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- logmean(p0 * q0, p1 * q1) / 
                    logmean(sum(p0 * q0, na.rm = TRUE), sum(p1 * q1, na.rm = TRUE))
                },
                Vartia2 = ,
                SatoVartia = function(p1, p0, q1, q0, na.rm = FALSE, scale = TRUE) {
                  stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
                              is_numeric(p1, p0, q1, q0),
                            "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
                              same_length(p1, p0, q1, q0),
                            "'scale' must be TRUE or FALSE" = 
                              length1(scale, "logical"))
                  res <- logmean(p0 * q0 / sum(p0 * q0, na.rm = TRUE), 
                                 p1 * q1 / sum(p1 * q1, na.rm = TRUE))
                })
  len <- length(body(res)) + 1L
  body(res)[len] <- expression(if (scale) weights_scale(res, na.rm) else res)
  res
}