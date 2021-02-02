#---- Price utilities----
offset_price <- function(type = c("back", "base")) {
  type <- match.arg(type)
  # return function
  function(x, period, product = rep(1, length(x))) {
    if (!same_length(x, period, product)) {
      stop("All arguments must be the same length")
    }
    if (!length(x)) return(x[0])
    offset <- function(x) x[c(1L, if (type == "back") seq_len(length(x) - 1))]
    period <- as.factor(period)
    price <- split(x, period)
    product <- split(product, period)
    warn <- any(vapply(product, anyDuplicated, numeric(1)) > 0)
    if (warn) {
      warning("There are duplicated period-product pairs") 
    }
    matches <- Map(match, product, offset(product), incomparables = NA)
    res <- unsplit(Map(`[`, offset(price), matches), period)
    attributes(res) <- attributes(x) # unsplit mangles attributes
    res
  }
}

back_price <- offset_price("back")

base_price <- offset_price("base")