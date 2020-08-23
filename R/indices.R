#---- Arithmetic index ----
index_arithmetic <- function(type = c("Carli", "Dutot", "Laspeyres",
                                      "Palgrave", "Drobish", "Unnamed",
                                      "Walsh1", "MarshallEdgeworth", "GearyKhamis",
                                      "Lowe", "Young")) {
  type <- match.arg(type)
  weights <- index_weights(type)
  switch(type,
         Carli = ,
         Dutot = function(p1, p0, na.rm = FALSE) {
           stopifnot("'p1' and 'p0' must be numeric vectors" = 
                       is_numeric(p1, p0),
                     "'p1' and 'p0' must be the same length" = 
                       same_length(p1, p0))
           mean_arithmetic(p1 / p0, weights(p0, scale = FALSE), na.rm)
         },
         Laspeyres = function(p1, p0, q0, na.rm = FALSE) {
           stopifnot("'p1', 'p0', and 'q0' must be numeric vectors" = 
                       is_numeric(p1, p0, q0),
                     "'p1', 'p0', and 'q0' must be the same length" = 
                       same_length(p1, p0, q0))
           mean_arithmetic(p1 / p0, weights(p0, q0, scale = FALSE), na.rm)
         },
         Palgrave = function(p1, p0, q1, na.rm = FALSE) {
           stopifnot("'p1', 'p0', and 'q1' must be numeric vectors" = 
                       is_numeric(p1, p0, q1),
                     "'p1', 'p0', and 'q1' must be the same length" = 
                       same_length(p1, p0, q1))
           mean_arithmetic(p1 / p0, weights(p1, q1, scale = FALSE), na.rm)
         },
         Drobish = ,
         Unnamed = function(p1, p0, q1, q0, na.rm = FALSE) {
           stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
                       is_numeric(p1, p0, q1, q0),
                     "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
                       same_length(p1, p0, q1, q0))
           mean_arithmetic(p1 / p0, weights(p1, p0, q1, q0, scale = FALSE), na.rm)
         },
         Walsh1 = ,
         MarshallEdgeworth = ,
         GearyKhamis = function(p1, p0, q1, q0, na.rm = FALSE) {
           stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
                       is_numeric(p1, p0, q1, q0),
                     "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
                       same_length(p1, p0, q1, q0))
           mean_arithmetic(p1 / p0, weights(p0, q1, q0, scale = FALSE), na.rm)
         },
         Lowe = function(p1, p0, qb, na.rm = FALSE) {
           stopifnot("'p1', 'p0', and 'qb' must be numeric vectors" = 
                       is_numeric(p1, p0, qb),
                     "'p1', 'p0', and 'qb' must be the same length" = 
                       same_length(p1, p0, qb))
           mean_arithmetic(p1 / p0, weights(p0, qb, scale = FALSE), na.rm)
         },
         Young = function(p1, p0, pb, qb, na.rm = FALSE) {
           stopifnot("'p1', 'p0', 'pb', and 'qb' must be numeric vectors" = 
                       is_numeric(p1, p0, pb, qb),
                     "'p1', 'p0', 'pb', and 'qb' must be the same length" = 
                       same_length(p1, p0, pb, qb))
           mean_arithmetic(p1 / p0, weights(pb, qb, scale = FALSE), na.rm)
         })
}

#---- Geometric index ----
index_geometric <- function(type = c("Jevons", "Laspeyres", "Paasche",
                                     "Tornqvist", "Vartia1", "MontgomeryVartia",
                                     "Vartia2", "SatoVartia", "Walsh2")) {
  type <- match.arg(type)
  weights <- index_weights(type)
  switch(type,
         Jevons = function(p1, p0, na.rm = FALSE) {
           stopifnot("'p1' and 'p0' must be numeric vectors" = 
                       is_numeric(p1, p0),
                     "'p1' and 'p0' must be the same length" = 
                       same_length(p1, p0))
           mean_geometric(p1 / p0, weights(p0, scale = FALSE), na.rm)
           },
         Laspeyres = function(p1, p0, q0, na.rm = FALSE) {
           stopifnot("'p1', 'p0', and 'q0' must be numeric vectors" = 
                       is_numeric(p1, p0, q0),
                     "'p1', 'p0', and 'q0' must be the same length" = 
                       same_length(p1, p0, q0))
           mean_geometric(p1 / p0, weights(p0, q0, scale = FALSE), na.rm)
           },
         Paasche = function(p1, p0, q1, na.rm = FALSE) {
           stopifnot("'p1', 'p0', and 'q1' must be numeric vectors" = 
                       is_numeric(p1, p0, q1),
                     "'p1', 'p0', and 'q1' must be the same length" = 
                       same_length(p1, p0, q1))
           mean_geometric(p1 / p0, weights(p1, q1, scale = FALSE), na.rm)
         },
         Vartia2 = ,
         SatoVartia = ,
         Walsh2 = ,
         Tornqvist = function(p1, p0, q1, q0, na.rm = FALSE) {
           stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
                       is_numeric(p1, p0, q1, q0),
                     "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
                       same_length(p1, p0, q1, q0))
           mean_geometric(p1 / p0, weights(p1, p0, q1, q0, scale = FALSE), na.rm)
         },
         Vartia1 = ,
         MontgomeryVartia = function(p1, p0, q1, q0, na.rm = FALSE) {
           stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
                       is_numeric(p1, p0, q1, q0),
                     "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
                       same_length(p1, p0, q1, q0))
           mean_geometric(p1 / p0, weights(p1, p0, q1, q0, scale = FALSE), na.rm, FALSE)
         })
}

#---- Harmonic index ----
index_harmonic <- function(type = c("Coggeshall", "Laspeyres", "Paasche")) {
  type <- match.arg(type)
  weights <- index_weights(type)
  switch(type,
         Coggeshall = function(p1, p0, na.rm = FALSE) {
           stopifnot("'p1' and 'p0' must be numeric vectors" = 
                       is_numeric(p1, p0),
                     "'p1' and 'p0' must be the same length" = 
                       same_length(p1, p0))
           mean_harmonic(p1 / p0, weights(p0, scale = FALSE), na.rm)
         },
         Laspeyres = function(p1, p0, q0, na.rm = FALSE) {
           stopifnot("'p1', 'p0', and 'q0' must be numeric vectors" = 
                       is_numeric(p1, p0, q0),
                     "'p1', 'p0', and 'q0' must be the same length" = 
                       same_length(p1, p0, q0))
           mean_harmonic(p1 / p0, weights(p0, q0, scale = FALSE), na.rm)
         },
         Paasche = function(p1, p0, q1, na.rm = FALSE) {
           stopifnot("'p1', 'p0', and 'q1' must be numeric vectors" = 
                       is_numeric(p1, p0, q1),
                     "'p1', 'p0', and 'q1' must be the same length" = 
                       same_length(p1, p0, q1))
           mean_harmonic(p1 / p0, weights(p1, q1, scale = FALSE), na.rm)
         })
}

#---- Common indexes ----
index_laspeyres <- index_arithmetic("Laspeyres")

index_paasche <- index_harmonic("Paasche")

index_lowe <- index_arithmetic("Lowe")

index_young <- index_arithmetic("Young")

#---- Fisher index ----
index_fisher <- function(p1, p0, q1, q0, na.rm = FALSE) {
  sqrt(index_laspeyres(p1, p0, q0, na.rm) * index_paasche(p1, p0, q1, na.rm))
}

#---- Harmonic Laspeyres Paasche index ----
index_hlp <- function(p1, p0, q1, q0, na.rm = FALSE) {
  2 / (1 / index_laspeyres(p1, p0, q0, na.rm) +
         1 / index_paasche(p1, p0, q1, na.rm))
}

#---- Lloyd Moulton index ----
index_lm <- function(p1, p0, q0, elasticity, na.rm = FALSE) {
  stopifnot("'p1', 'p0', and 'q0' must be numeric vectors" = 
              is_numeric(p1, p0, q0),
            "'p1', 'p0', and 'q0' must be the same length" = 
              same_length(p1, p0, q0))
  weights <- index_weights("LloydMoulton")
  mean_generalized(1 - elasticity)(p1 / p0, weights(p0, q0, scale = FALSE), na.rm)
}

#---- Caruthers Sellwood Ward Dalen index ----
index_cswd <- function(p1, p0, na.rm = FALSE) {
  stopifnot( "'p1' and 'p0' must be numeric vectors" = 
               is_numeric(p1, p0),
             "'p1' and 'p0' must be the same length" = 
               same_length(p1, p0))
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) *
         mean_harmonic(p1 / p0, na.rm = na.rm))
}

#---- Caruthers Sellwood Ward Dalen Balk index ----
index_cswdb <- function(p1, p0, q1, q0, na.rm = FALSE) {
  stopifnot("'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
              is_numeric(p1, p0, q1, q0),
            "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
              same_length(p1, p0, q1, q0))
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) /
         mean_arithmetic(q1 / q0, na.rm = na.rm) *
         mean_arithmetic(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

#---- Balk Walsh index ----
index_bw <- function(p1, p0, na.rm = FALSE) {
  stopifnot("'p1' and 'p0' must be numeric vectors" = 
              is_numeric(p1, p0),
            "'p1' and 'p0' must be the same length" = 
              same_length(p1, p0))
  mean_arithmetic(sqrt(p1 / p0), na.rm = na.rm) *
    mean_harmonic(sqrt(p1 / p0), na.rm = na.rm)
}

#---- Generalized Stuval index ----
index_stuval <- function(p1, p0, q1, q0, a, b, na.rm = FALSE) {
  stopifnot("'a' must be a length 1 numeric vector" = length1(a, "numeric"),
            "'b' must be a length 1 numeric vector" = length1(b, "numeric"))
  pl <- index_laspeyres(p1, p0, q0, na.rm)
  ql <- index_laspeyres(q1, q0, p0, na.rm)
  v <- sum(p1 * q1, na.rm = na.rm) / sum(p0 * q0, na.rm = na.rm)
  (pl - b / a * ql) / 2 + sqrt((pl - b / a * ql)^2 / 4 + b / a * v)
}