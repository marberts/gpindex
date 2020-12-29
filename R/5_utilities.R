#---- Back price ----
back_price <- function(x, period, product = rep(1, length(x))) {
  stopifnot("All arguments must be atomic vectors" = all_atomic(x, period, product),
            "All arguments must be the same length" = all_same_length(x, period, product))
  if (!length(x)) return(x[0])
  offset <- function(x) x[c(1L, seq_len(length(x) - 1))]
  period <- as.factor(period)
  price <- split(x, period)
  product <- split(product, period)
  warn <- any(vapply(product, anyDuplicated, numeric(1)) > 0)
  if (warn) {
    warning("There are duplicated period-product pairs; back price not defined") 
  }
  matches <- Map(match, product, offset(product), incomparables = NA)
  res <- unsplit(Map(`[`, offset(price), matches), period)
  attributes(res) <- attributes(x) # unsplit mangles attributes
  res
}