#---- Arithmetic index ----
index_arithmetic <- function(type = c("Carli", "Dutot", "Laspeyres",
                                      "Palgrave", "Drobish", "Unnamed",
                                      "Walsh1", "MarshallEdgeworth", "GearyKhamis",
                                      "Lowe", "Young")) {
  type <- match.arg(type)
  weights <- index_weights(type)
  res <- switch(type,
    Carli = ,
    Dutot = function(p1, p0, na.rm = FALSE)
      mean_arithmetic(p1 / p0, weights(p0), na.rm),
    Laspeyres = function(p1, p0, q0, na.rm = FALSE) 
      mean_arithmetic(p1 / p0, weights(p0, q0), na.rm),
    Palgrave = function(p1, p0, q1, na.rm = FALSE)
      mean_arithmetic(p1 / p0, weights(p1, q1), na.rm),
    Drobish = ,
    Unnamed = function(p1, p0, q1, q0, na.rm = FALSE)
      mean_arithmetic(p1 / p0, weights(p1, p0, q1, q0), na.rm),
    Walsh1 = ,
    MarshallEdgeworth = ,
    GearyKhamis = function(p1, p0, q1, q0, na.rm = FALSE)
      mean_arithmetic(p1 / p0, weights(p0, q1, q0), na.rm),
    Lowe = function(p1, p0, qb, na.rm = FALSE)
      mean_arithmetic(p1 / p0, weights(p0, qb), na.rm),
    Young = function(p1, p0, pb, qb, na.rm = FALSE)
      mean_arithmetic(p1 / p0, weights(pb, qb), na.rm)
  )
  # all arguments except na.rm are price and quantity arguments
  # extract them in a list
  pqs <- lapply(setdiff(names(formals(res)), c("na.rm")), as.name)
  # insert argument checking in body of res
  body(res) <- as.call(c(quote(`{`), call("stopifnot"), body(res)))
  errors <- c("prices/quantities must be numeric vectors",
              "prices/quantites must be the same length")
  body(res)[[2]][errors] <- list(as.call(c(quote(is_numeric), pqs)),
                                 as.call(c(quote(same_length), pqs)))
  res
}

#---- Geometric index ----
index_geometric <- function(type = c("Jevons", "Laspeyres", "Paasche",
                                     "Tornqvist", "Vartia1", "MontgomeryVartia",
                                     "Vartia2", "SatoVartia", "Walsh2",
                                     "Young")) {
  type <- match.arg(type)
  weights <- index_weights(type)
  res <- switch(type,
    Jevons = function(p1, p0, na.rm = FALSE)
      mean_geometric(p1 / p0, weights(p0), na.rm),
    Laspeyres = function(p1, p0, q0, na.rm = FALSE)
      mean_geometric(p1 / p0, weights(p0, q0), na.rm),
    Paasche = function(p1, p0, q1, na.rm = FALSE)
      mean_geometric(p1 / p0, weights(p1, q1), na.rm),
    Vartia2 = ,
    SatoVartia = ,
    Walsh2 = ,
    Tornqvist = function(p1, p0, q1, q0, na.rm = FALSE)
      mean_geometric(p1 / p0, weights(p1, p0, q1, q0), na.rm),
    Vartia1 = ,
    MontgomeryVartia = function(p1, p0, q1, q0, na.rm = FALSE)
      mean_geometric(p1 / p0, weights(p1, p0, q1, q0), na.rm, FALSE),
    Young = function(p1, p0, pb, qb, na.rm = FALSE)
      mean_geometric(p1 / p0, weights(pb, qb), na.rm)
  )
  # all arguments except na.rm are price and quantity arguments
  # extract them in a list
  pqs <- lapply(setdiff(names(formals(res)), c("na.rm")), as.name)
  # insert argument checking in body of res
  body(res) <- as.call(c(quote(`{`), call("stopifnot"), body(res)))
  errors <- c("prices/quantities must be numeric vectors",
              "prices/quantites must be the same length")
  body(res)[[2]][errors] <- list(as.call(c(quote(is_numeric), pqs)),
                                 as.call(c(quote(same_length), pqs)))
  res
}

#---- Harmonic index ----
index_harmonic <- function(type = c("Coggeshall", "Laspeyres", "Paasche")) {
  type <- match.arg(type)
  weights <- index_weights(type)
  res <- switch(type,
    Coggeshall = function(p1, p0, na.rm = FALSE)
      mean_harmonic(p1 / p0, weights(p0), na.rm),
    Laspeyres = function(p1, p0, q0, na.rm = FALSE)
      mean_harmonic(p1 / p0, weights(p0, q0), na.rm),
    Paasche = function(p1, p0, q1, na.rm = FALSE)
      mean_harmonic(p1 / p0, weights(p1, q1), na.rm)
  )
  # all arguments except na.rm are price and quantity arguments
  # extract them in a list
  pqs <- lapply(setdiff(names(formals(res)), c("na.rm")), as.name)
  # insert argument checking in body of res
  body(res) <- as.call(c(quote(`{`), call("stopifnot"), body(res)))
  errors <- c("prices/quantities must be numeric vectors",
              "prices/quantites must be the same length")
  body(res)[[2]][errors] <- list(as.call(c(quote(is_numeric), pqs)),
                                 as.call(c(quote(same_length), pqs)))
  res
}

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
  stopifnot("prices/quantities must be numeric vectors" = is_numeric(p1, p0, q0),
            "prices/quantities must be be the same length" = same_length(p1, p0, q0))
  weights <- index_weights("LloydMoulton")
  mean_generalized(1 - elasticity)(p1 / p0, weights(p0, q0), na.rm)
}

#---- Caruthers Sellwood Ward Dalen index ----
index_cswd <- function(p1, p0, na.rm = FALSE) {
  stopifnot("prices/quantities must be numeric vectors" = is_numeric(p1, p0),
            "prices/quantities must be the same length" = same_length(p1, p0))
  rel <- p1 / p0
  sqrt(mean_arithmetic(rel, na.rm = na.rm) * mean_harmonic(rel, na.rm = na.rm))
}

#---- Caruthers Sellwood Ward Dalen Balk index ----
index_cswdb <- function(p1, p0, q1, q0, na.rm = FALSE) {
  stopifnot("prices/quantities must be numeric vectors" = is_numeric(p1, p0, q1, q0),
            "prices/quantities must be the same length" = same_length(p1, p0, q1, q0))
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) /
         mean_arithmetic(q1 / q0, na.rm = na.rm) *
         mean_arithmetic(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

#---- Balk Walsh index ----
index_bw <- function(p1, p0, na.rm = FALSE) {
  stopifnot("prices/quantities must be numeric vectors" = is_numeric(p1, p0),
            "prices/quantities must be numeric vectors" = same_length(p1, p0))
  rel <- sqrt(p1 / p0)
  mean_arithmetic(rel, na.rm = na.rm) * mean_harmonic(rel, na.rm = na.rm)
}

#---- Generalized Stuval index ----
index_stuval <- function(a, b) {
  stopifnot("'a' must be a length 1 numeric vector" = length1(a, "numeric"),
            "'b' must be a length 1 numeric vector" = length1(b, "numeric"))
  function(p1, p0, q1, q0, na.rm = FALSE) {
    pl <- index_laspeyres(p1, p0, q0, na.rm)
    ql <- index_laspeyres(q1, q0, p0, na.rm)
    v <- sum(p1 * q1, na.rm = na.rm) / sum(p0 * q0, na.rm = na.rm)
    (pl - b / a * ql) / 2 + sqrt((pl - b / a * ql)^2 / 4 + b / a * v)
  }
}