index_arithmetic <- function(p1, p0, q1, q0, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm)
  type <- match.arg(type, types$arithmetic_index_types)
  w <- index_weights(p1, p0, q1, q0, type, scale = FALSE)
  mean_arithmetic(p1 / p0, w, na.rm)
}

index_lowe <- function(p1, p0, qb, na.rm = FALSE) {
  check_index_arguments(p1, p0, q0 = qb, na.rm = na.rm)
  w <- index_weights(p0 = p0, q0 = qb, type = "Lowe", scale = FALSE)
  mean_arithmetic(p1 / p0, w, na.rm)
}

index_young <- function(p1, p0, pb, qb, na.rm = FALSE) {
  check_index_arguments(p1, p0, q0 = qb, na.rm = na.rm)
  stopifnot(
    "pb must be numeric" = is.numeric(pb),
    "pb and p0 must be the same length" = length(pb) == length(p0)
  )
  w <- index_weights(p0 = pb, q0 = qb, type = "Young", scale = FALSE)
  mean_arithmetic(p1 / p0, w, na.rm)
}

index_geometric <- function(p1, p0, q1, q0, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm)
  type <- match.arg(type, types$geometric_index_types)
  w <- index_weights(p1, p0, q1, q0, type, scale = FALSE)
  mean_geometric(p1 / p0, w, na.rm, 
                 scale = !type %in% c("Vartia1", "MontgomeryVartia"))
}

index_harmonic <- function(p1, p0, q1, q0, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm)
  type <- match.arg(type, types$harmonic_index_types)
  w <- index_weights(p1, p0, q1, q0, type, scale = FALSE)
  mean_harmonic(p1 / p0, w, na.rm)
}

index_fisher <- function(p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm)
  sqrt(index_arithmetic(p1, p0, q1, q0, "Laspeyres", na.rm) *
         index_harmonic(p1, p0, q1, q0, "Paasche",  na.rm))
}

index_hlp <- function(p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm)
  2 / (1 / index_arithmetic(p1, p0, q1, q0, "Laspeyres", na.rm) +
         1 / index_harmonic(p1, p0, q1, q0, "Paasche", na.rm))
}

index_lm <- function(p1, p0, q0, elasticity, na.rm = FALSE) {
  check_index_arguments(p1, p0, q0 = q0, na.rm = na.rm)
  w <- index_weights(p1, p0, q0 = q0, type = "LloydMoulton", scale = FALSE)
  mean_generalized(p1 / p0, w, 1 - elasticity, na.rm)
}

index_cswd <- function(p1, p0, na.rm = FALSE) {
  check_index_arguments(p1, p0, na.rm = na.rm)
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) *
         mean_harmonic(p1 / p0, na.rm = na.rm))
}

index_cswdb <- function(p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm)
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) /
         mean_arithmetic(q1 / q0, na.rm = na.rm) *
         mean_arithmetic(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

index_bw <- function(p1, p0, na.rm = FALSE) {
  check_index_arguments(p1, p0, na.rm = na.rm)
  mean_arithmetic(sqrt(p1 / p0), na.rm = na.rm) *
    mean_harmonic(sqrt(p1 / p0), na.rm = na.rm)
}