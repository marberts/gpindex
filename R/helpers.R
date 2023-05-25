# None of these functions are exported
#---- Argument checking ----
not_number <- function(x) {
  length(x) != 1L || !is.numeric(x) || !is.finite(x)
}

different_lengths <- function(...) {
  res <- lengths(list(...))
  any(res != res[1L])
}

duplicate_products <- function(x) {
  any(vapply(x, anyDuplicated, numeric(1L), incomparables = NA) > 0)
}
