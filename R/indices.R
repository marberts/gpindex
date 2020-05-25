index_arithmetic <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types$arithmetic_index_types)
  w <- if (type == "Lowe") {
    index_weights(p0 = p0, q0 = qb, type = type)
  } else if (type == "Young") {
    index_weights(p0 = pb, q0 = qb, type = type)
  } else {
    index_weights(p1, p0, q1, q0, type)
  }
  mean_arithmetic(p1 / p0, w, na.rm)
}

index_geometric <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types$geometric_index_types)
  w <- if (type == "Young") {
    index_weights(p0 = pb, q0 = qb, type = type)
  } else {
    index_weights(p1, p0, q1, q0, type)
  }
  mean_geometric(p1 / p0, w, na.rm, scale = !type %in% c("Vartia1", "MontgomeryVartia"))
}

index_harmonic <- function (p1, p0, q1, q0, pb, qb, type, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, pb, qb, type, na.rm)
  type <- match.arg(type, types$harmonic_index_types)
  w <- if (type == "Young") {
    index_weights(p0 = pb, q0 = qb, type = type)
  } else {
    index_weights(p1, p0, q1, q0, type)
  }
  mean_harmonic(p1 / p0, w, na.rm)
}

index_fisher <- function (p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  sqrt(index_arithmetic(p1, p0, q1, q0, type = "Laspeyres", na.rm = na.rm) *
         index_harmonic(p1, p0, q1, q0, type = "Paasche", na.rm = na.rm))
}

index_hlp <- function (p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  2 / (1 / index_arithmetic(p1, p0, q1, q0, type = "Laspeyres", na.rm = na.rm) +
         1 / index_harmonic(p1, p0, q1, q0, type = "Paasche", na.rm = na.rm))
}

index_lm <- function (p1, p0, q1, q0, elasticity, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  w <- index_weights(p1, p0, q1, q0, type = "Laspeyres")
  mean_generalized(p1 / p0, w, 1 - elasticity, na.rm)
}

index_cswd <- function (p1, p0, na.rm = FALSE) {
  check_index_arguments(p1, p0, na.rm = na.rm)
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) * 
         mean_harmonic(p1 / p0, na.rm = na.rm))
}

index_cswdb <- function (p1, p0, q1, q0, na.rm = FALSE) {
  check_index_arguments(p1, p0, q1, q0, na.rm = na.rm)
  sqrt(mean_arithmetic(p1 / p0, na.rm = na.rm) / 
         mean_arithmetic(q1 / q0, na.rm = na.rm) *  
         mean_arithmetic(p1 * q1 / (p0 * q0), na.rm = na.rm))
}

index_bw <- function (p1, p0, na.rm = FALSE) {
  check_index_arguments(p1, p0, na.rm = na.rm)
  mean_arithmetic(sqrt(p1 / p0), na.rm = na.rm) * 
    index_harmonic(sqrt(p1 / p0), na.rm = na.rm)
}
