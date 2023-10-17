#' Test that an input is a finite scalar
#' @noRd
not_finite_scalar <- function(x) {
  length(x) != 1L || !is.finite(x)
}

#' Test that an input is a finite pair of scalars
#' @noRd
not_finite_pair <- function(x) {
  length(x) != 2L || !all(is.finite(x))
}

#' Test that an inputs are the same length
#' @noRd
different_lengths <- function(...) {
  res <- lengths(list(...))
  any(res != res[1L])
}

#' Test that there are duplicate products
#' @noRd
duplicate_products <- function(x) {
  any(vapply(x, anyDuplicated, numeric(1L), incomparables = NA) > 0)
}
