#---- Contributions ----
contributions <- function(r) {
  arithmetic_weights <- transmute_weights(r, 1)
  # return function
  res <- function(x, w) {
    scale_weights(arithmetic_weights(x, w)) * (x - 1)
  }
  # clean up enclosing environment
  enc <- list(arithmetic_weights = arithmetic_weights)
  environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
  res
}

arithmetic_contributions <- contributions(1)

geometric_contributions <- contributions(0)

harmonic_contributions <- contributions(-1)

#---- Nested contributions ----
nc <- function(method) {
  nest_transmute <- switch(method, nested_transmute, nested_transmute2)
  function(r1, r2, t = c(1, 1)) {
    arithmetic_weights <- nest_transmute(r1, r2, 1, t)
    # return function
    res <- function(x, w1, w2) {
      scale_weights(arithmetic_weights(x, w1, w2)) * (x - 1)
    }
    # clean up enclosing environment
    enc <- list(arithmetic_weights = arithmetic_weights)
    environment(res) <- list2env(enc, parent = getNamespace("gpindex"))
    res
  }
}
nested_contributions <- nc(1)

nested_contributions2 <- nc(2)

fisher_contributions <- nested_contributions(0, c(1, -1))

fisher_contributions2 <- nested_contributions2(0, c(1, -1))