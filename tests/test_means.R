# Some data for tests
set.seed(1234)
x <- rlnorm(15)
xna <- rlnorm(15)
xna[sample(15, 4)] <- NA_real_
w <- runif(15)
a <- runif(15, 0, 5)
b <- rlnorm(15)

#---- Tests for generalized means ----
stopifnot(
  exprs = {
    # Simple checks against known values
    all.equal(mean_arithmetic(1:5), 3)
    all.equal(mean_arithmetic(1:4, 4:1), 2)
    all.equal(mean_arithmetic(1:4, scale = FALSE), 10)
    all.equal(mean_arithmetic(1:4, 4:1, scale = FALSE), 20)
    all.equal(mean_geometric(c(1, 4)), 2)
    all.equal(mean_geometric(1:3, 1:3), prod((1:3)^(1:3 / 6)))
    all.equal(mean_harmonic(1:2), 4 / 3)
    all.equal(mean_harmonic(1:3, 1:3), 2)
    # Is the fundamental inequality satisfied?
    mean_geometric(1:10, 10:1) <= mean_arithmetic(1:10, 10:1)
    mean_geometric(1:10, 10:1) >= mean_harmonic(1:10, 10:1)
    # Tests against a simple implementation with stats::weighted.mean and base::mean
    all(
      vapply(
        seq(-10, 10, by = 0.25),
        function(r)
          abs(
            c(
              mean_generalized(r)(x, w) -
                if (r != 0) {
                  (stats::weighted.mean(x^r, w))^(1 / r) 
                } else {
                  exp(stats::weighted.mean(log(x), w))
                },
              mean_generalized(r)(x) -
                if (r != 0) (mean(x^r))^(1 / r) else exp(mean(log(x)))
            )
          ) < .Machine$double.eps^0.5,
        logical(2)
      )
    )
    # Is the general inequality satisfied?
    all(
      vapply(
        seq(-10, 10, by = 0.25),
        function(r) {
          mean_generalized(r - runif(1, 0, 5))(x, w) <= 
            mean_generalized(r)(x, w)
          },
        logical(1)
      )
    )
    # Checks for NA_real_ and length-0 inputs
    is.na(mean_arithmetic(NA_real_))
    is.na(mean_arithmetic(NA_real_, 1))
    is.na(mean_arithmetic(NA_real_, 0.5, scale = FALSE))
    is.na(mean_arithmetic(1, NA_real_))
    is.na(mean_arithmetic(NaN))
    is.na(mean_arithmetic(NaN, 1))
    is.na(mean_arithmetic(1, NaN))
    is.na(mean_arithmetic(NA_real_, na.rm = TRUE))
    is.na(mean_arithmetic(NaN, na.rm = TRUE))
    is.na(mean_arithmetic(NA_real_, 1, na.rm = TRUE))
    mean_arithmetic(NA_real_, 0.5, na.rm = TRUE, scale = FALSE) == 0
    is.na(mean_arithmetic(1, NA_real_, na.rm = TRUE))
    is.na(mean_arithmetic(1, NaN, na.rm = TRUE))
    is.na(mean_arithmetic(numeric(0)))
    is.na(mean_arithmetic(numeric(0), numeric(0)))
    all.equal(mean_arithmetic(c(1, NA_real_), na.rm = TRUE), 1)
    all.equal(mean_arithmetic(1:2, c(2, NA_real_), na.rm = TRUE), 1)
    all.equal(mean_arithmetic(c(1, NA_real_), c(1, 2), na.rm = TRUE), 1)
    is.na(mean_arithmetic(c(1, NA_real_), c(NA_real_, 2), na.rm = TRUE))
    all.equal(mean_arithmetic(1:2, c(2, NA_real_), na.rm = TRUE, scale = FALSE), 2)
    mean_arithmetic(c(1, NA_real_), c(NA_real_, 2), na.rm = TRUE, scale = FALSE) == 0
    # Change weights
    all(
      apply(
        expand.grid(a = seq(-3, 3, by = 0.25), b = seq(-3, 3, by = 0.25)),
        1,
        function(p) {
          c(
            abs(mean_generalized(p[1])(x, w) -
                  mean_generalized(p[2])(x, weights_transmute(p[1], p[2])(x, w))),
            abs(mean_generalized(p[1])(x) -
                  mean_generalized(p[2])(x, weights_transmute(p[1], p[2])(x))),
            abs(mean_generalized(p[1])(xna, w, TRUE) -
                  mean_generalized(p[2])(xna, weights_transmute(p[1], p[2])(xna, w), TRUE))
          ) < .Machine$double.eps^0.5
        }
      )
    )
    abs(mean_geometric(c(2, sqrt(2)^2)) -
          mean_arithmetic(c(2, sqrt(2)^2), weights_transmute(0, 1)(c(2, sqrt(2)^2)))) < .Machine$double.eps^0.5
    # Factor weights
    all(
      vapply(
        seq(-5, 5, by = 0.25),
        function(r)
          c(
            abs(mean_generalized(r)(x * a, w) -
                  mean_generalized(r)(x, w) * mean_generalized(r)(a, weights_factor(r)(x, w))),
            abs(mean_generalized(r)(x * a) -
                  mean_generalized(r)(x) * mean_generalized(r)(a, weights_factor(r)(x))),
            abs(mean_generalized(r)(xna * a, w, TRUE) -
                  mean_generalized(r)(xna, w, TRUE) *
                  mean_generalized(r)(a, weights_factor(r)(xna, w), TRUE))
          ) < .Machine$double.eps^0.5,
        logical(3)
      )
    )
    # Zero values
    all(
      vapply(
        seq(-10, 0, by = 0.5), 
        function(r) mean_generalized(r)(replace(x, 3, 0), w) == 0,
        logical(1)
        )
      )
    all.equal(mean_geometric(c(1, 0, 2), c(0.5, 0, 0.5)), sqrt(2))
    all.equal(mean_harmonic(c(1, -1, 2), c(0.5, 0, 0.5)), 4/3)
    mean_arithmetic(c(1, Inf), c(1, 0)) == 1
    is.na(mean_arithmetic(c(1, NaN), c(1, 0)))
  },
  local = getNamespace("gpindex")
)

#---- Tests for generalized log means ----
stopifnot(
  exprs = {
    # Checks against known values
    logmean(1, 1) == 1
    logmean(1, 0) == 0
    all.equal(logmean(2, 1), 1 / log(2))
    logmean_generalized(2)(1, 2) == 1.5
    all.equal(logmean_generalized(-1)(1, 2), sqrt(2))
    all.equal(mean_extended(0, 0)(4, 4), 4)
    # Test against a simple implementation
    all(
      vapply(
        seq(-10, 10, by = 0.25),
        function(r)
          all(
            abs(
              logmean_generalized(r)(a, b) -
                if (r == 0) {
                  (b - a) / log(b / a)
                } else if (r == 1) {
                  1 / exp(1) * (b^b / a^a)^(1 / (b - a))
                } else {
                  ((b^r - a^r) / (r * (b - a)))^(1 / (r - 1))
                }
            ) < .Machine$double.eps^0.5
          ),
        logical(1)
      )
    )
    # Test symmetry
    all(
      vapply(
        seq(-5, 5, by = 0.25),
        function(r)
          all(
            abs(
              mean_extended(r, 2.5)(a, b) - mean_extended(r, 2.5)(b, a)
              ) < .Machine$double.eps^0.5
          ),
        logical(1)
      )
    )
    # Checks for NA_real_ and length-0 inputs
    identical(logmean(numeric(0), numeric(0)), numeric(0))
    identical(logmean(1:5, numeric(0)), numeric(0))
    identical(logmean(numeric(0), 5:1), numeric(0))
    is.na(logmean(1, NA_real_))
    is.na(logmean(NA_real_, 1))
    is.na(logmean(NA_real_, NA_real_))
    is.na(logmean(NA_real_, NaN))
    is.na(logmean(NaN, NA_real_))
    is.na(logmean(1, NaN))
    is.na(logmean(NaN, 1))
    is.na(logmean(NaN, NaN))
    # Test of a == b
    logmean(0.00001, 1 / 100000) == 1 / 100000
    logmean(2, sqrt(2)^2) == 2
    logmean_generalized(0.9)(2, sqrt(2)^2) == 2
    logmean_generalized(1.1)(2, sqrt(2)^2) == 2
    # Test of recycling
    all(logmean(1, 1:5) == logmean(c(1, 1, 1, 1, 1), 1:5))
    all(logmean(1:2, 1:5) == logmean(c(1, 2, 1, 2, 1), 1:5))
    all(logmean(1:5, 1:2) == logmean(c(1, 2, 1, 2, 1), 1:5))
    all(logmean(2:3, rep(sqrt(2)^2, 5)) == c(2, logmean(3, sqrt(2)^2), 2, logmean(3, sqrt(2)^2), 2))
    # Some identities
    all.equal(logmean_generalized(-1)(a, b),
              apply(matrix(c(a, b), ncol = 2), 1, mean_geometric))
    all.equal(logmean_generalized(2)(a, b),
              apply(matrix(c(a, b), ncol = 2), 1, mean_arithmetic))
    all.equal(logmean_generalized(-2)(a, b),
              apply(matrix(c(a, b), ncol = 2), 1, function(x) (mean_harmonic(x) * mean_geometric(x)^2)^(1 / 3)))
    all.equal(logmean_generalized(0.5)(a, b),
              apply(matrix(c(a, b), ncol = 2), 1, function(x) (mean_arithmetic(x) + mean_geometric(x)) / 2))
    all.equal(logmean(a, b),
              apply(matrix(c(a, b), ncol = 2), 1, mean_geometric)^2 * logmean(1 / a, 1 / b))
    all.equal(mean_extended(-2, -1)(a, b), apply(matrix(c(a, b), ncol = 2), 1, mean_harmonic))
    all.equal(mean_extended(-2, 2)(a, b), apply(matrix(c(a, b), ncol = 2), 1, mean_geometric))
    all.equal(mean_extended(3.5, -3.5)(a, b), apply(matrix(c(a, b), ncol = 2), 1, mean_geometric))
    all.equal(mean_extended(2, 2)(a, b), (a^a^2 / b^b^2)^(1 / (a^2 - b^2)) / exp(1)^(1 / 2))
    },
  local = getNamespace("gpindex")
)
