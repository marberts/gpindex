# Some data for tests
set.seed(4321)
x <- rnorm(15)^2
w <- runif(15, 0, 2)
xna <- rlnorm(15)
xna[sample(xna, 3)] <- NA

#---- Tests for weights_change ----
stopifnot(
  exprs = {
    diff(weights_change(x, r = 2, k = 2)) == 0
    # Test against a simple implementation
    all(
      apply(
        expand.grid(a = seq(-10, 10, by = 0.5), b = seq(-10, 10, by = 0.5)), 
        1,
        function(p) {
          w2 <- weights_change(x, w, p[1], p[2]) 
          M <- mean_generalized(x, w, p[1])
          w3 <- w * logmean_generalized(x, M, p[1])^(p[1] - 1) / logmean_generalized(x, M, p[2])^(p[2] - 1)
          abs(w2 / sum(w2) - w3 / sum(w3)) < .Machine$double.eps^0.5
        }
      )
    )
    # check that it works with small differences
    all(abs(weights_change(rep.int(sqrt(2)^2, length(x)), r = 0.9, k = 1.1) - 
          weights_change(rep.int(sqrt(2)^2, length(x)), r = 0.9, k = 1.1, M = 2)) < .Machine$double.eps^0.5)
    # length 0 inputs
    length(weights_g2a(numeric(0))) == 0L
    length(weights_g2a(numeric(0), numeric(0))) == 0L
    # NA inputs
    is.na(weights_g2a(NA_real_))
    is.na(weights_g2a(NA_real_, 1))
    is.na(weights_g2a(1, NA_real_))
    is.nan(weights_g2a(NaN, 1))
    is.nan(weights_g2a(1, NaN))
    is.na(weights_g2a(NA_real_, na.rm = TRUE))
    is.nan(weights_g2a(NaN, na.rm = TRUE))
    identical(weights_g2a(c(1, NA_real_)), c(NA_real_, NA_real_))
    identical(weights_g2a(c(1, NaN)), c(NA, NaN))
    identical(weights_g2a(c(1, NA_real_), na.rm = TRUE), c(1, NA))
    identical(weights_g2a(c(1, NaN), na.rm = TRUE), c(1, NaN))
  },
  local = getNamespace("gpindex")
)

#---- Test for weights_factor ----
stopifnot(
  exprs = {
    # test against known cases
    all(weights_factor(x, r = 0) == 1)
    all(weights_factor(x, w, r = 0) == w)
    # test against a simple implementation
    all(
      vapply(
        seq(-10, 10, by = 0.25), 
        function(r) {
          w2 <- weights_factor(x, w, r)
          w3 <- w * x^r
          all(abs(w2 / sum(w2) - w3 / sum(w3)) < .Machine$double.eps^0.5)
        }, 
        logical(1)
      )
    )
  },
  local = getNamespace("gpindex")
)