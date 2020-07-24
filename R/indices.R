#---- Arithmetic index ----
index_arithmetic <- function(p1, p0, q1, q0, 
                             type = c("Carli", "Dutot", "Laspeyres",
                                      "Palgrave", "Drobish", "Unnamed",
                                      "Walsh1", "MarshallEdgeworth", "GearyKhamis"), 
                             na.rm = FALSE) {
  stopifnot(
    "'p1' and 'p0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric"),
    "'q1' must be a numeric vector" = 
      missing(q1) || is.vector(q1, "numeric"),
    "'q0' must be a numeric vector" = 
      missing(q0) || is.vector(q0, "numeric"),
    "'p1' and 'p0' must be the same length" = 
      length(p1) == length(p0),
    "'q1' must be the same length as 'p1' and 'p0'" = 
      missing(q1) || length(p0) == length(q1),
    "'q0' must be the same length as 'p1' and 'p0'" = 
      missing(q0) || length(p0) == length(q0),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  type <- match.arg(type)
  w <- index_weights(p1, p0, q1, q0, type, scale = FALSE)
  mean_arithmetic(p1 / p0, w, na.rm)
}

#---- Lowe index ----
index_lowe <- function(p1, p0, qb, na.rm = FALSE) {
  stopifnot(
    "'p1', 'p0', and 'qb' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric") && is.vector(qb, "numeric"),
    "'p1', 'p0', and 'qb' must be the same length" = 
      length(p1) == length(p0) && length(p0) == length(qb),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  w <- index_weights(p0 = p0, q0 = qb, type = "Lowe", scale = FALSE)
  mean_arithmetic(p1 / p0, w, na.rm)
}

#---- Young index ----
index_young <- function(p1, p0, pb, qb, na.rm = FALSE) {
  stopifnot(
    "'p1', 'p0', 'pb', and 'qb' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric") && 
      is.vector(pb, "numeric") && is.vector(qb, "numeric"),
    "'p1', 'p0', 'pb', and 'qb' must be the same length" = 
      length(p1) == length(p0) && length(pb) == length(p0) && length(qb) == length(p0),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  w <- index_weights(p0 = pb, q0 = qb, type = "Young", scale = FALSE)
  mean_arithmetic(p1 / p0, w, na.rm)
}

#---- Geometric index ----
index_geometric <- function(p1, p0, q1, q0, 
                            type = c("Jevons", "Laspeyres", "Paasche",
                                     "Tornqvist", "Vartia1", "MontgomeryVartia",
                                     "Vartia2", "SatoVartia", "Walsh2"), 
                            na.rm = FALSE) {
  stopifnot(
    "'p1' and 'p0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric"),
    "'q1' must be a numeric vector" = 
      missing(q1) || is.vector(q1, "numeric"),
    "'q0' must be a numeric vector" = 
      missing(q0) || is.vector(q0, "numeric"),
    "'p1' and 'p0' must be the same length" = 
      length(p1) == length(p0),
    "'q1' must be the same length as 'p1' and 'p0'" = 
      missing(q1) || length(p0) == length(q1),
    "'q0' must be the same length as 'p1' and 'p0'" = 
      missing(q0) || length(p0) == length(q0),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  type <- match.arg(type)
  w <- index_weights(p1, p0, q1, q0, type, scale = FALSE)
  mean_geometric(p1 / p0, w, na.rm, 
                 scale = !type %in% c("Vartia1", "MontgomeryVartia"))
}

#---- Harmonic index ----
index_harmonic <- function(p1, p0, q1, q0, 
                           type = c("Coggeshall", "Laspeyres", "Paasche"), 
                           na.rm = FALSE) {
  stopifnot(
    "'p1' and 'p0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric"),
    "'q1' must be a numeric vector" = 
      missing(q1) || is.vector(q1, "numeric"),
    "'q0' must be a numeric vector" = 
      missing(q0) || is.vector(q0, "numeric"),
    "'p1' and 'p0' must be the same length" = 
      length(p1) == length(p0),
    "'q1' must be the same length as 'p1' and 'p0'" = 
      missing(q1) || length(p0) == length(q1),
    "'q0' must be the same length as 'p1' and 'p0'" = 
      missing(q0) || length(p0) == length(q0),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  type <- match.arg(type)
  w <- index_weights(p1, p0, q1, q0, type, scale = FALSE)
  mean_harmonic(p1 / p0, w, na.rm)
}

#---- Fisher index ----
index_fisher <- function(p1, p0, q1, q0, na.rm = FALSE) {
  stopifnot(
    "'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric") && 
      is.vector(q1, "numeric") && is.vector(q0, "numeric"),
    "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
      length(p1) == length(p0) && length(q1) == length(p0) && length(q0) == length(p0),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  sqrt(index_arithmetic(p1, p0, q1, q0, "Laspeyres", na.rm) *
         index_harmonic(p1, p0, q1, q0, "Paasche",  na.rm))
}

#---- Harmonic Laspeyres Paasche index ----
index_hlp <- function(p1, p0, q1, q0, na.rm = FALSE) {
  stopifnot(
    "'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric") && 
      is.vector(q1, "numeric") && is.vector(q0, "numeric"),
    "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
      length(p1) == length(p0) && length(q1) == length(p0) && length(q0) == length(p0),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  2 / (1 / index_arithmetic(p1, p0, q1, q0, "Laspeyres", na.rm) +
         1 / index_harmonic(p1, p0, q1, q0, "Paasche", na.rm))
}

#---- Lloyd Moulton index ----
index_lm <- function(p1, p0, q0, elasticity, na.rm = FALSE) {
  stopifnot(
    "'p1', 'p0', and 'q0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric") && is.vector(q0, "numeric"),
    "'p1', 'p0', and 'q0' must be the same length" = 
      length(p1) == length(p0) && length(q0) == length(p0),
    "'elasticity' must be a length 1 numeric vector" = 
      length(elasticity) == 1L && is.vector(elasticity, "numeric"),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  w <- index_weights(p1, p0, q0 = q0, type = "LloydMoulton", scale = FALSE)
  mean_generalized(p1 / p0, w, 1 - elasticity, na.rm)
}

#---- Caruthers Sellwood Ward Dalen index ----
index_cswd <- function(p1, p0, na.rm = FALSE) {
  stopifnot(
    "'p1' and 'p0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric"),
    "'p1' and 'p0' must be the same length" = 
      length(p1) == length(p0),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) *
         mean_harmonic(p1 / p0, na.rm = na.rm))
}

#---- Caruthers Sellwood Ward Dalen Balk index ----
index_cswdb <- function(p1, p0, q1, q0, na.rm = FALSE) {
  stopifnot(
    "'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric") && 
      is.vector(q1, "numeric") && is.vector(q0, "numeric"),
    "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
      length(p1) == length(p0) && length(q1) == length(p0) && length(q0) == length(p0),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) /
         mean_arithmetic(q1 / q0, na.rm = na.rm) *
         mean_arithmetic(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

#---- Balk Walsh index ----
index_bw <- function(p1, p0, na.rm = FALSE) {
  stopifnot(
    "'p1' and 'p0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric"),
    "'p1' and 'p0' must be the same length" = 
      length(p1) == length(p0),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  mean_arithmetic(sqrt(p1 / p0), na.rm = na.rm) *
    mean_harmonic(sqrt(p1 / p0), na.rm = na.rm)
}

#---- Generalized Stuval index ----
index_stuval <- function(p1, p0, q1, q0, a, b, na.rm = FALSE) {
  stopifnot(
    "'p1', 'p0', 'q1', and 'q0' must be numeric vectors" = 
      is.vector(p1, "numeric") && is.vector(p0, "numeric") && 
      is.vector(q1, "numeric") && is.vector(q0, "numeric"),
    "'p1', 'p0', 'q1', and 'q0' must be the same length" = 
      length(p1) == length(p0) && length(q1) == length(p0) && length(q0) == length(p0),
    "'a' and 'b' must be length 1 numeric vectors" = 
      length(a) == 1L && length(b) == 1L &&
      is.vector(a, "numeric") && is.vector(b, "numeric"),
    "'na.rm' must be TRUE or FALSE" = 
      length(na.rm) == 1L && is.logical(na.rm) && !is.na(na.rm)
  )
  pl <- index_arithmetic(p1, p0, q0 = q0, type = "Laspeyres", na.rm = na.rm)
  ql <- index_arithmetic(q1, q0, q0 = p0, type = "Laspeyres", na.rm = na.rm)
  v <- sum(p1 * q1, na.rm = na.rm) / sum(p0 * q0, na.rm = na.rm)
  (pl - b / a * ql) / 2 + sqrt((pl - b / a * ql)^2 / 4 + b / a * v)
}