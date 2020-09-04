# Some data for tests
set.seed(4321)
x <- rnorm(15)^2
w <- runif(15, 0, 2)

#---- Tests for weights_change ----
stopifnot(
  exprs = {
    all(diff(weights_change(2, 2)(x)) == 0)
    all.equal(weights_change(-2, -2)(x, w), w)
    !anyNA(weights_change(1, 1)(c(1, NA_real_)))
    anyNA(weights_change(2, 1)(c(1, NA_real_)))
    # Test against a simple implementation
    all(
      apply(
        expand.grid(a = seq(-10, 10, by = 0.5), b = seq(-10, 10, by = 0.5)),
        1,
        function(p) {
          w2 <- weights_change(p[1], p[2])(x, w)
          M <- mean_generalized(p[1])(x, w)
          w3 <- w * logmean_generalized(p[1])(x, M)^(p[1] - 1) /
            logmean_generalized(p[2])(x, M)^(p[2] - 1)
          abs(weights_scale(w2) - weights_scale(w3)) < .Machine$double.eps^0.5
        }
      )
    )
    # length 0 inputs
    length(weights_g2a(numeric(0))) == 0L
    length(weights_g2a(numeric(0), numeric(0))) == 0L
    # NA_real_ inputs
    is.na(weights_g2a(NA_real_))
    is.na(weights_g2a(NA_real_, 1))
    is.na(weights_g2a(1, NA_real_))
    is.na(weights_g2a(NaN))
    is.na(weights_g2a(NaN, 1))
    is.na(weights_g2a(1, NaN))
    is.na(weights_g2a(NA_real_, NA_real_))
    is.na(weights_g2a(NaN, NaN))
    is.na(weights_g2a(NA_real_, NaN))
    is.na(weights_g2a(NaN, NA_real_))
    identical(is.na(weights_g2a(c(1, NA_real_))), c(FALSE, TRUE))
    identical(is.na(weights_g2a(c(1, NaN))), c(FALSE, TRUE))
  },
  local = getNamespace("gpindex")
)

#---- Test for weights_factor ----
stopifnot(
  exprs = {
    anyNA(weights_factor(0)(c(1, NA_real_)))
    # test against known cases
    all(weights_factor(0)(x) == 1)
    all(weights_factor(0)(x, w) == w)
    all(weights_factor(1)(x, w) == x * w)
    all(weights_update(x, w) == x * w)
    # test against a simple implementation
    all(
      vapply(
        seq(-10, 10, by = 0.25),
        function(r) {
          w2 <- weights_factor(r)(x, w)
          w3 <- w * x^r
          all.equal(w2, w3)
        },
        logical(1)
      )
    )
    # test NA_real_ and length-0 inputs
    is.na(weights_update(NA_real_))
    is.na(weights_update(NA_real_, NA_real_))
    is.na(weights_update(NaN))
    is.na(weights_update(NaN, NaN))
    is.na(weights_update(NA_real_, NaN))
    is.na(weights_update(NaN, NA_real_))
    is.na(weights_update(1, NaN))
    is.na(weights_update(1, NA_real_))
    is.na(weights_update(NA_real_, 1))
    is.na(weights_update(NaN, 1))
    length(weights_update(numeric(0))) == 0L
    length(weights_update(numeric(0), numeric(0))) == 0L
  },
  local = getNamespace("gpindex")
)

#---- Test for weights_scale ----
stopifnot(
  exprs = {
    all.equal(weights_scale(1:4), 1:4 / 10)
    all.equal(sum(weights_scale(w)), 1)
    all.equal(weights_scale(c(1:2, NA_real_)), c(1:2, NA_real_) / 3)
    all.equal(sum(weights_scale(c(1:2, NA_real_)), na.rm = TRUE), 1)
    length(weights_scale(numeric(0))) == 0L
  },
  local = getNamespace("gpindex")
)