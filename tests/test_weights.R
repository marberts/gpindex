# Some data for tests
set.seed(4321)
x <- rnorm(15)^2
w <- runif(15, 0, 2)

#---- Tests for weights_transmute ----
stopifnot(
  exprs = {
    all(diff(weights_transmute(2, 2)(x)) == 0)
    all.equal(weights_transmute(-2, -2)(x, w), w)
    anyNA(weights_transmute(1, 1)(c(1, NA_real_)))
    anyNA(weights_transmute(2, 1)(c(1, NA_real_)))
    # Test against a simple implementation
    all(
      apply(
        expand.grid(a = seq(-10, 10, by = 0.5), b = seq(-10, 10, by = 0.5)),
        1,
        function(p) {
          w2 <- weights_transmute(p[1], p[2])(x, w)
          M <- mean_generalized(p[1])(x, w)
          w3 <- w * logmean_generalized(p[1])(x, M)^(p[1] - 1) /
            logmean_generalized(p[2])(x, M)^(p[2] - 1)
          abs(weights_scale(w2) - weights_scale(w3)) < .Machine$double.eps^0.5
        }
      )
    )
    # length 0 inputs
    length(weights_transmute(1, 0)(numeric(0))) == 0L
    length(weights_transmute(1, 0)(numeric(0), numeric(0))) == 0L
    # NA_real_ inputs
    is.na(weights_transmute(1, 0)(NA_real_))
    is.na(weights_transmute(1, 0)(NA_real_, 1))
    is.na(weights_transmute(1, 0)(1, NA_real_))
    is.na(weights_transmute(1, 0)(NaN))
    is.na(weights_transmute(1, 0)(NaN, 1))
    is.na(weights_transmute(1, 0)(1, NaN))
    is.na(weights_transmute(1, 0)(NA_real_, NA_real_))
    is.na(weights_transmute(1, 0)(NaN, NaN))
    is.na(weights_transmute(1, 0)(NA_real_, NaN))
    is.na(weights_transmute(1, 0)(NaN, NA_real_))
    identical(is.na(weights_transmute(1, 0)(c(1, NA_real_))), c(FALSE, TRUE))
    identical(is.na(weights_transmute(1, 0)(c(1, NaN))), c(FALSE, TRUE))
  },
  local = getNamespace("gpindex")
)

#---- Tests for contributions ----
stopifnot(
  exprs = {
    all(
      vapply(
        seq(-10, 10, by = 0.25),
        function(r) {
          x <- replace(x, 1, NA)
          con <- contributions(r)(x, w)
          all.equal(sum(con, na.rm = TRUE), mean_generalized(r)(x, w, na.rm = TRUE) - 1)
        },
        logical(1)
      )
    )
    all.equal(contributions_arithmetic(1:4), c(0, 0.25, 0.5, 0.75))
    all.equal(contributions_harmonic(1:4), c(0, 0.24, 0.32, 0.36))
    all.equal(contributions_geometric(c(1, 4)), c(0, 1))
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