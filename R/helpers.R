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

#' Check price and quantity arguments
#' @noRd
check_pqs <- function(...) {
  vars <- as.character(match.call()[-1L])
  if (different_lengths(...)) {
    stop(gettextf("%s must be the same length", toString(sQuote(vars, "'"))))
  }
}

#' Make a list of balanced product indexes over time
#' @noRd
balance_products <- function(product, period) {
  ux <- unique(product)
  product <- split(product, period)
  if (duplicate_products(product)) {
    warning("there are duplicated period-product pairs")
  }
  lapply(product, \(x) match(ux, x, incomparables = NA))
}
