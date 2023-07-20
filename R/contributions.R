#---- Contributions ----
contributions <- function(r) {
  arithmetic_weights <- transmute_weights(r, 1)

  function(x, w = NULL) {
    (x - 1) * scale_weights(arithmetic_weights(x, w))
  }
}

arithmetic_contributions <- contributions(1)

geometric_contributions <- contributions(0)

harmonic_contributions <- contributions(-1)

#---- Nested contributions ----
nc <- function(nest_transmute) {
  nest_transmute <- match.fun(nest_transmute)

  function(r1, r2, t = c(1, 1)) {
    arithmetic_weights <- nest_transmute(r1, r2, 1, t)

    function(x, w1 = NULL, w2 = NULL) {
      (x - 1) * scale_weights(arithmetic_weights(x, w1, w2))
    }
  }
}

nested_contributions <- nc(nested_transmute)

nested_contributions2 <- nc(nested_transmute2)

fisher_contributions <- nested_contributions(0, c(1, -1))

fisher_contributions2 <- nested_contributions2(0, c(1, -1))
