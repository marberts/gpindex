index_weights <- function(type = c("Carli", "Jevons", "Coggeshall",
                                   "Dutot", "Laspeyres", "HybridLaspeyres",
                                   "Palgrave", "Paasche", "HybridPaasche",
                                   "Drobish", "Unnamed", "Tornqvist",
                                   "Walsh1", "Walsh2", "MarshallEdgeworth",
                                   "GearyKhamis", "Vartia1", "MontgomeryVartia",
                                   "Vartia2", "SatoVartia", "Lowe",
                                   "Young", "LloydMoulton")) {
  # make function for weights formulas
  res <- switch(
    match.arg(type),
    Carli = ,
    Jevons = ,
    Coggeshall = function(p0) rep(1, length(p0)),
    Dutot = function(p0) p0,
    Young = function(pb, qb) pb * qb,
    Lowe = function(p0, qb) p0 * qb,
    LloydMoulton = ,
    Laspeyres = function(p0, q0) p0 * q0,
    HybridLaspeyres = function(p1, q0) p1 * q0,
    Palgrave = ,
    Paasche = function(p1, q1) p1 * q1,
    HybridPaasche = function(p0, q1) p0 * q1,
    Drobish = function(p1, p0, q1, q0)
      (p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 
         p0 * q1 / sum(p0 * q1, na.rm = TRUE)) / 2,
    Unnamed = ,
    Tornqvist = function(p1, p0, q1, q0)
      (p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 
         p1 * q1 / sum(p1 * q1, na.rm = TRUE)) / 2,
    Walsh1 = function(p0, q1, q0) p0 * sqrt(q0 * q1),
    Walsh2 = function(p1, p0, q1, q0) sqrt(p0 * q0 * p1 * q1),
    MarshallEdgeworth = function(p0, q1, q0) p0 * (q0 + q1),
    GearyKhamis = function(p0, q1, q0) p0 / (1 / q0 + 1 / q1),
    Vartia1 = ,
    MontgomeryVartia = function(p1, p0, q1, q0)
      logmean(p0 * q0, p1 * q1) / 
      logmean(sum(p0 * q0, na.rm = TRUE), sum(p1 * q1, na.rm = TRUE)),
    Vartia2 = ,
    SatoVartia = function(p1, p0, q1, q0)
      logmean(p0 * q0 / sum(p0 * q0, na.rm = TRUE), 
              p1 * q1 / sum(p1 * q1, na.rm = TRUE))
  )
  # all arguments are price and quantity argument, so extract them in a list
  pqs <- lapply(names(formals(res)), as.name)
  # insert argument checking in body of res
  body(res) <- as.call(c(quote(`{`), call("stopifnot"), body(res)))
  errors <- c("prices/quantities must be numeric vectors",
              "prices/quantites must be the same length")
  body(res)[[2]][errors] <- list(as.call(c(quote(is_numeric), pqs)),
                                 as.call(c(quote(same_length), pqs)))
  res
}
