#---- Index weights ----
index_weights <- function(type = c("Carli", "Jevons", "Coggeshall",
                                   "Dutot", "Laspeyres", "HybridLaspeyres",
                                   "Palgrave", "Paasche", "HybridPaasche",
                                   "Drobish", "Unnamed", "Tornqvist",
                                   "Walsh1", "Walsh2", "MarshallEdgeworth",
                                   "GearyKhamis", "Vartia1", "MontgomeryVartia",
                                   "Vartia2", "SatoVartia", "Lowe",
                                   "Young", "LloydMoulton")) {
  # function for weights formulas
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
              "prices/quantities must be the same length")
  body(res)[[2]][errors] <- list(as.call(c(quote(all_numeric), pqs)),
                                 as.call(c(quote(all_same_length), pqs)))
  res
}

#---- Pythagorean indexes ----
index_pythagorean <- function(class = c("arithmetic", "geometric", "harmonic")) {
  types <- switch(match.arg(class),
                  arithmetic = c("Carli", "Dutot", "Laspeyres",
                                 "Palgrave", "Drobish", "Unnamed",
                                 "Walsh1", "MarshallEdgeworth", "GearyKhamis",
                                 "Lowe", "Young"),
                  geometric = c("Jevons", "Laspeyres", "Paasche",
                                "Tornqvist", "Vartia1", "MontgomeryVartia",
                                "Vartia2", "SatoVartia", "Walsh2",
                                "Young"),
                  harmonic = c("Coggeshall", "Laspeyres", "Paasche", "Young"))
  r <- switch(class, arithmetic = 1, geometric = 0, harmonic = -1)
  generalized_mean <- mean_generalized(r)
  # return function
  function(type) {
    type <- match.arg(type, types)
    weights <- index_weights(type)
    res <- switch(type,
                  Carli = ,
                  Dutot = ,
                  Jevons = ,
                  Coggeshall = function(p1, p0, na.rm = FALSE)
                    generalized_mean(p1 / p0, weights(p0), na.rm),
                  Laspeyres = function(p1, p0, q0, na.rm = FALSE) 
                    generalized_mean(p1 / p0, weights(p0, q0), na.rm),
                  Paasche = ,
                  Palgrave = function(p1, p0, q1, na.rm = FALSE)
                    generalized_mean(p1 / p0, weights(p1, q1), na.rm),
                  Drobish = ,
                  Unnamed = ,
                  Vartia2 = ,
                  SatoVartia = ,
                  Walsh2 = ,
                  Tornqvist = function(p1, p0, q1, q0, na.rm = FALSE)
                    generalized_mean(p1 / p0, weights(p1, p0, q1, q0), na.rm),
                  Vartia1 = ,
                  MontgomeryVartia = function(p1, p0, q1, q0, na.rm = FALSE)
                    generalized_mean(p1 / p0, weights(p1, p0, q1, q0), na.rm, FALSE),
                  Walsh1 = ,
                  MarshallEdgeworth = ,
                  GearyKhamis = function(p1, p0, q1, q0, na.rm = FALSE)
                    generalized_mean(p1 / p0, weights(p0, q1, q0), na.rm),
                  Lowe = function(p1, p0, qb, na.rm = FALSE)
                    generalized_mean(p1 / p0, weights(p0, qb), na.rm),
                  Young = function(p1, p0, pb, qb, na.rm = FALSE)
                    generalized_mean(p1 / p0, weights(pb, qb), na.rm)
    )
    # all arguments except na.rm are price and quantity arguments
    # extract them in a list
    pqs <- lapply(setdiff(names(formals(res)), "na.rm"), as.name)
    # insert argument checking in body of res
    body(res) <- as.call(c(quote(`{`), call("stopifnot"), body(res)))
    errors <- c("prices/quantities must be numeric vectors",
                "prices/quantities must be the same length")
    body(res)[[2]][errors] <- list(as.call(c(quote(all_numeric), pqs)),
                                   as.call(c(quote(all_same_length), pqs)))
    res
  }
}

index_arithmetic <- index_pythagorean("arithmetic")

index_geometric <- index_pythagorean("geometric")

index_harmonic <- index_pythagorean("harmonic")

#---- Common indexes ----
index_laspeyres <- index_arithmetic("Laspeyres")

index_paasche <- index_harmonic("Paasche")

index_jevons <- index_geometric("Jevons")

index_lowe <- index_arithmetic("Lowe")

index_young <- index_arithmetic("Young")

#---- Fisher index ----
index_fisher <- function(p1, p0, q1, q0, na.rm = FALSE) {
  sqrt(index_laspeyres(p1, p0, q0, na.rm) * index_paasche(p1, p0, q1, na.rm))
}

#---- Harmonic Laspeyres Paasche index ----
index_hlp <- function(p1, p0, q1, q0, na.rm = FALSE) {
  2 / (1 / index_laspeyres(p1, p0, q0, na.rm) + 1 / index_paasche(p1, p0, q1, na.rm))
}

#---- Lloyd Moulton index ----
index_lm <- function(p1, p0, q0, elasticity, na.rm = FALSE) {
  stopifnot("prices/quantities must be numeric vectors" = all_numeric(p1, p0, q0),
            "prices/quantities must be be the same length" = all_same_length(p1, p0, q0))
  weights <- index_weights("LloydMoulton")
  mean_generalized(1 - elasticity)(p1 / p0, weights(p0, q0), na.rm)
}

#---- Caruthers Sellwood Ward Dalen index ----
index_cswd <- function(p1, p0, na.rm = FALSE) {
  stopifnot("prices/quantities must be numeric vectors" = all_numeric(p1, p0),
            "prices/quantities must be the same length" = all_same_length(p1, p0))
  rel <- p1 / p0
  sqrt(mean_arithmetic(rel, na.rm = na.rm) * mean_harmonic(rel, na.rm = na.rm))
}

#---- Caruthers Sellwood Ward Dalen Balk index ----
index_cswdb <- function(p1, p0, q1, q0, na.rm = FALSE) {
  stopifnot("prices/quantities must be numeric vectors" = all_numeric(p1, p0, q1, q0),
            "prices/quantities must be the same length" = all_same_length(p1, p0, q1, q0))
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) /
         mean_arithmetic(q1 / q0, na.rm = na.rm) *
         mean_arithmetic(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

#---- Balk Walsh index ----
index_bw <- function(p1, p0, na.rm = FALSE) {
  stopifnot("prices/quantities must be numeric vectors" = all_numeric(p1, p0),
            "prices/quantities must be numeric vectors" = all_same_length(p1, p0))
  rel <- sqrt(p1 / p0)
  mean_arithmetic(rel, na.rm = na.rm) * mean_harmonic(rel, na.rm = na.rm)
}

#---- Generalized Stuval index ----
index_stuval <- function(a, b) {
  stopifnot("'a' must be a length 1 numeric" = is_number(a),
            "'b' must be a length 1 numeric" = is_number(b))
  function(p1, p0, q1, q0, na.rm = FALSE) {
    pl <- index_laspeyres(p1, p0, q0, na.rm)
    ql <- index_laspeyres(q1, q0, p0, na.rm)
    v <- sum(p1 * q1, na.rm = na.rm) / sum(p0 * q0, na.rm = na.rm)
    (pl - b / a * ql) / 2 + sqrt((pl - b / a * ql)^2 / 4 + b / a * v)
  }
}