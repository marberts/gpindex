# Some data for tests
set.seed(4321)
x <- rnorm(15)^2
w <- runif(15, 0, 2)

#---- Tests for weights_change ----
stopifnot(
  exprs = {
    diff(weights_change(x, r = 2, k = 2)) == 0
    all(weights_change(x, w, -2 , -2, scale = FALSE) == w)
    any(weights_change(x, w, -2, 3, M = mean_generalized(x, w, -2) + 1) !=
               weights_change(x, w, -2, 3))
    !anyNA(weights_change(c(1, NA), r = 1, k = 1))
    anyNA(weights_change(c(1, NA), r = 2, k = 1))
    # Test against a simple implementation
    all(
      apply(
        expand.grid(a = seq(-10, 10, by = 0.5), b = seq(-10, 10, by = 0.5)),
        1,
        function(p) {
          w2 <- weights_change(x, w, p[1], p[2])
          M <- mean_generalized(x, w, p[1])
          w3 <- w * logmean_generalized(x, M, p[1])^(p[1] - 1) /
            logmean_generalized(x, M, p[2])^(p[2] - 1)
          abs(w2 - weights_scale(w3)) < .Machine$double.eps^0.5
        }
      )
    )
    # check that it works with small differences
    all.equal(weights_change(rep.int(sqrt(2)^2, length(x)), r = 0.9, k = 1.1),
          weights_change(rep.int(sqrt(2)^2, length(x)), r = 0.9, k = 1.1, M = 2))
    # length 0 inputs
    length(weights_g2a(numeric(0))) == 0L
    length(weights_g2a(numeric(0), numeric(0))) == 0L
    # NA inputs
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
    is.na(weights_g2a(NA_real_, na.rm = TRUE))
    is.na(weights_g2a(NaN, na.rm = TRUE))
    is.na(weights_g2a(numeric(0)))
    is.na(weights_g2a(1:5, 5:1, M = NaN))
    is.na(weights_g2a(1:5, 5:1, M = Inf))
    is.na(weights_g2a(1:5, 5:1, M = NA_real_))
    is.na(weights_g2a(1:5, 5:1, na.rm = TRUE, M = NA_real_))
    identical(is.na(weights_g2a(c(1, NA_real_))), c(TRUE, TRUE))
    identical(is.na(weights_g2a(c(1, NaN))), c(TRUE, TRUE))
    identical(is.na(weights_g2a(c(1, NA_real_), na.rm = TRUE)), c(FALSE, TRUE))
    identical(is.na(weights_g2a(c(1, NaN), na.rm = TRUE)), c(FALSE, TRUE))
  },
  local = getNamespace("gpindex")
)

#---- Test for weights_factor ----
stopifnot(
  exprs = {
    anyNA(weights_factor(c(1, NA), r = 0))
    # test against known cases
    all(weights_factor(x, r = 0) == 1 / length(x))
    all(weights_factor(x, w, r = 0, scale = FALSE) == w)
    all(weights_factor(x, w, r = 1) == x * w / sum(x * w))
    all(weights_update(x, w) == x * w / sum(x * w))
    # test against a simple implementation
    all(
      vapply(
        seq(-10, 10, by = 0.25),
        function(r) {
          w2 <- weights_factor(x, w, r)
          w3 <- w * x^r
          all.equal(w2, weights_scale(w3))
        },
        logical(1)
      )
    )
    # test NA and length-0 inputs
    is.na(weights_factor(NA_real_, r = 1))
    is.na(weights_factor(NA_real_, NA_real_, r = 1))
    is.na(weights_factor(NaN, r = 1))
    is.na(weights_factor(NaN, NaN, r = 1))
    is.na(weights_factor(NA_real_, NaN, r = 1))
    is.na(weights_factor(NaN, NA_real_, r = 1))
    is.na(weights_factor(1, NaN, r = 1))
    is.na(weights_factor(1, NA_real_, r = 1))
    is.na(weights_factor(NA_real_, 1, r = 1))
    is.na(weights_factor(NaN, 1, r = 1))
    length(weights_factor(numeric(0), r = 1)) == 0L
    length(weights_factor(numeric(0), numeric(0), r = 1)) == 0L
  },
  local = getNamespace("gpindex")
)

#---- Test for weights_scale ----
stopifnot(
  exprs = {
    all.equal(weights_scale(1:4), 1:4 / 10)
    all.equal(sum(weights_scale(w)), 1)
    is.na(weights_scale(c(1:2, NA)))
    is.na(weights_scale(c(1:2, NaN)))
    all.equal(weights_scale(c(1:2, NA), TRUE), c(1:2, NA) / 3)
    length(weights_scale(numeric(0))) == 0L
  },
  local = getNamespace("gpindex")
)