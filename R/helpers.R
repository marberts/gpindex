# None of these functions are exported
not_finite_scalar <- function(x) {
  length(x) != 1L || !is.finite(x)
}

not_finite_pair <- function(x) {
  length(x) != 2L || !all(is.finite(x))
}

different_lengths <- function(...) {
  res <- lengths(list(...))
  any(res != res[1L])
}

duplicate_products <- function(x) {
  any(vapply(x, anyDuplicated, numeric(1L), incomparables = NA) > 0)
}
