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
    Coggeshall = function(p0, na.rm = FALSE, scale = TRUE) 
      res <- rep(1, length(p0)),
    Dutot = function(p0, na.rm = FALSE, scale = TRUE) 
      res <- p0,
    Young = ,
    Lowe = ,
    LloydMoulton = ,
    Laspeyres = function(p0, q0, na.rm = FALSE, scale = TRUE) 
      res <- p0 * q0,
    HybridLaspeyres = function(p1, q0, na.rm = FALSE, scale = TRUE) 
      res <- p1 * q0,
    Palgrave = ,
    Paasche = function(p1, q1, na.rm = FALSE, scale = TRUE) 
      res <- p1 * q1,
    HybridPaasche = function(p0, q1, na.rm = FALSE, scale = TRUE) 
      res <- p0 * q1,
    Drobish = function(p1, p0, q1, q0, na.rm = FALSE, scale = TRUE)
      res <- (p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 
                p0 * q1 / sum(p0 * q1, na.rm = TRUE)) / 2,
    Unnamed = ,
    Tornqvist = function(p1, p0, q1, q0, na.rm = FALSE, scale = TRUE)
      res <- (p0 * q0 / sum(p0 * q0, na.rm = TRUE) + 
                p1 * q1 / sum(p1 * q1, na.rm = TRUE)) / 2,
    Walsh1 = function(p0, q1, q0, na.rm = FALSE, scale = TRUE)
      res <- p0 * sqrt(q0 * q1),
    Walsh2 = function(p1, p0, q1, q0, na.rm = FALSE, scale = TRUE) 
      res <- sqrt(p0 * q0 * p1 * q1),
    MarshallEdgeworth = function(p0, q1, q0, na.rm = FALSE, scale = TRUE)
      res <- p0 * (q0 + q1),
    GearyKhamis = function(p0, q1, q0, na.rm = FALSE, scale = TRUE) 
      res <- p0 / (1 / q0 + 1 / q1),
    Vartia1 = ,
    MontgomeryVartia = function(p1, p0, q1, q0, na.rm = FALSE, scale = FALSE)
      res <- logmean(p0 * q0, p1 * q1) / 
      logmean(sum(p0 * q0, na.rm = TRUE), sum(p1 * q1, na.rm = TRUE)),
    Vartia2 = ,
    SatoVartia = function(p1, p0, q1, q0, na.rm = FALSE, scale = TRUE)
      res <- logmean(p0 * q0 / sum(p0 * q0, na.rm = TRUE), 
                     p1 * q1 / sum(p1 * q1, na.rm = TRUE))
  )
  # all arguments except na.rm and scale are price and quantity arguments
  # extract them in a list
  pqs <- lapply(setdiff(names(formals(res)), c("na.rm", "scale")), as.name)
  # insert argument checking in body of res
  body(res) <- as.call(c(quote(`{`), call("stopifnot"), body(res)))
  errors <- c("prices/quantities must be numeric vectors",
              "prices/quantites must be the same length",
              "'scale' must be TRUE or FALSE")
  body(res)[[2]][errors] <- list(as.call(c(quote(is_numeric), pqs)),
                                 as.call(c(quote(same_length), pqs)),
                                 call("length1", quote(scale), "logical"))
  # line 4 is always the last line of the body
  body(res)[4] <- expression(if (scale) weights_scale(res, na.rm) else res)
  res
}
