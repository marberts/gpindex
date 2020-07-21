# Some data for tests
set.seed(1234)
x <- rlnorm(15)
xna <- rlnorm(15)
xna[sample(15, 4)] <- NA
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
    # Test with logicals
    all.equal(mean_arithmetic(c(TRUE, FALSE)), 0.5)
    all.equal(mean_arithmetic(c(TRUE, FALSE), scale = FALSE), 1)
    all.equal(mean_arithmetic(c(TRUE, FALSE), c(FALSE, TRUE)), 0)
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
              mean_generalized(x, w, r) -
                if (r != 0) {
                  (stats::weighted.mean(x^r, w))^(1 / r) 
                } else {
                  exp(stats::weighted.mean(log(x), w))
                },
              mean_generalized(x, r = r) -
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
          mean_generalized(x, w, r - runif(1, 0, 5)) <= 
            mean_generalized(x, w, r)
          },
        logical(1)
      )
    )
    # Checks for NA and length-0 inputs
    is.na(mean_arithmetic(NA))
    is.na(mean_arithmetic(NA, 1))
    is.na(mean_arithmetic(NA, 0.5, scale = FALSE))
    is.na(mean_arithmetic(1, NA))
    is.na(mean_arithmetic(NaN))
    is.na(mean_arithmetic(NaN, 1))
    is.na(mean_arithmetic(1, NaN))
    is.na(mean_arithmetic(NA, na.rm = TRUE))
    is.na(mean_arithmetic(NaN, na.rm = TRUE))
    is.na(mean_arithmetic(NA, 1, na.rm = TRUE))
    mean_arithmetic(NA, 0.5, na.rm = TRUE, scale = FALSE) == 0
    is.na(mean_arithmetic(1, NA, na.rm = TRUE))
    is.na(mean_arithmetic(1, NaN, na.rm = TRUE))
    is.na(mean_arithmetic(numeric(0)))
    is.na(mean_arithmetic(numeric(0), numeric(0)))
    all.equal(mean_arithmetic(c(1, NA), na.rm = TRUE), 1)
    all.equal(mean_arithmetic(1:2, c(2, NA), na.rm = TRUE), 1)
    all.equal(mean_arithmetic(c(1, NA), c(1, 2), na.rm = TRUE), 1)
    is.na(mean_arithmetic(c(1, NA), c(NA, 2), na.rm = TRUE))
    all.equal(mean_arithmetic(1:2, c(2, NA), na.rm = TRUE, scale = FALSE), 2)
    mean_arithmetic(c(1, NA), c(NA, 2), na.rm = TRUE, scale = FALSE) == 0
    # Change weights
    all(
      apply(
        expand.grid(a = seq(-3, 3, by = 0.25), b = seq(-3, 3, by = 0.25)),
        1,
        function(p) {
          c(
            abs(mean_generalized(x, w, p[1]) -
                  mean_generalized(x, weights_change(x, w, p[1], p[2]), p[2])),
            abs(mean_generalized(x, r = p[1]) -
                  mean_generalized(x, weights_change(x, r = p[1], k = p[2]), p[2])),
            abs(mean_generalized(xna, w, p[1], na.rm = TRUE) -
                  mean_generalized(xna, weights_change(xna, w, p[1], p[2], na.rm = TRUE), p[2], na.rm = TRUE))
          ) < .Machine$double.eps^0.5
        }
      )
    )
    abs(mean_geometric(c(2, sqrt(2)^2)) -
          mean_arithmetic(c(2, sqrt(2)^2), weights_g2a(c(2, sqrt(2)^2)))) < .Machine$double.eps^0.5
    # Factor weights
    all(
      vapply(
        seq(-5, 5, by = 0.25),
        function(r)
          c(
            abs(mean_generalized(x * a, w, r) -
                  mean_generalized(x, w, r) * mean_generalized(a, weights_factor(x, w, r), r)),
            abs(mean_generalized(x * a, r = r) -
                  mean_generalized(x, r = r) * mean_generalized(a, weights_factor(x, r = r), r)),
            abs(mean_generalized(xna * a, w, r, TRUE) -
                  mean_generalized(xna, w, r, TRUE) *
                  mean_generalized(a, weights_factor(xna, w, r, TRUE), r, TRUE))
          ) < .Machine$double.eps^0.5,
        logical(3)
      )
    )
    # Zero values
    all(
      vapply(
        seq(-10, 0, by = 0.5), 
        function(r) mean_generalized(replace(x, 3, 0), w, r = r) == 0,
        logical(1)
        )
      )
    all.equal(mean_geometric(c(1, 0, 2), c(0.5, 0, 0.5)), sqrt(2))
    all.equal(mean_harmonic(c(1, -1, 2), c(0.5, 0, 0.5)), 4/3)
    mean_arithmetic(c(1, Inf), c(1, 0)) == 1
    is.na(mean_arithmetic(c(1, NaN), c(1, 0)))
    # Limits
    mean_generalized(1:5, r = Inf) == 5
    mean_generalized(1:5, r = -Inf) == 1
    mean_generalized(1:5, 5:1, r = Inf) == 5
    mean_generalized(1:5, 5:1, r = -Inf) == 1
    mean_generalized(c(2:3, NA), r = Inf, na.rm = TRUE) == 3
    mean_generalized(c(2:3, NA), r = -Inf, na.rm = TRUE) == 2
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
    logmean_generalized(1, 2, 2) == 1.5
    all.equal(logmean_generalized(1, 2, -1), sqrt(2))
    # Test against a simple implementation
    all(
      vapply(
        seq(-10, 10, by = 0.25),
        function(r)
          all(
            abs(
              logmean_generalized(a, b, r) -
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
              logmean_generalized(a, b, r) - logmean_generalized(b, a, r)
              ) < .Machine$double.eps^0.5
          ),
        logical(1)
      )
    )
    # Checks for NA and length-0 inputs
    identical(logmean(numeric(0), numeric(0)), numeric(0))
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
    logmean_generalized(2, sqrt(2)^2, 0.9) == 2
    logmean_generalized(2, sqrt(2)^2, 1.1) == 2
    # Test of recycling
    logmean(1, 1:5) == logmean(c(1, 1, 1, 1, 1), 1:5)
    # Some identities
    all.equal(logmean_generalized(a, b, -1),
              apply(matrix(c(a, b), ncol = 2), 1, mean_geometric))
    all.equal(logmean_generalized(a, b, 2),
              apply(matrix(c(a, b), ncol = 2), 1, mean_arithmetic))
    all.equal(logmean_generalized(a, b, -2),
              apply(matrix(c(a, b), ncol = 2), 1, function(x) (mean_harmonic(x) * mean_geometric(x)^2)^(1 / 3)))
    all.equal(logmean_generalized(a, b, 0.5),
              apply(matrix(c(a, b), ncol = 2), 1, function(x) (mean_arithmetic(x) + mean_geometric(x)) / 2))
    all.equal(logmean(a, b),
              apply(matrix(c(a, b), ncol = 2), 1, mean_geometric)^2 * logmean(1 / a, 1 / b))
   },
  local = getNamespace("gpindex")
)