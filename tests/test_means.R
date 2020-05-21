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
    abs(mean_arithmetic(1:5) - 3) < .Machine$double.eps^0.5
    abs(mean_arithmetic(1:4, 4:1) - 2) < .Machine$double.eps^0.5
    abs(mean_arithmetic(1:4, scale = FALSE) - 10) < .Machine$double.eps^0.5
    abs(mean_arithmetic(1:4, 4:1, scale = FALSE) - 20) < .Machine$double.eps^0.5
    abs(mean_geometric(c(1, 4)) - 2) < .Machine$double.eps^0.5
    abs(mean_geometric(1:3, 1:3) - prod((1:3)^(1:3 / 6))) < .Machine$double.eps^0.5
    abs(mean_harmonic(1:2) - 4 / 3) < .Machine$double.eps^0.5
    abs(mean_harmonic(1:3, 1:3) - 2) < .Machine$double.eps^0.5
    # Is the fundamental inequality satisfied?
    mean_geometric(1:100, 100:1) <= mean_arithmetic(1:100, 100:1)
    mean_geometric(1:100, 100:1) >= mean_harmonic(1:100, 100:1)
    # Tests against a simple implementation with stats::weighted.mean
    all(
      vapply(
        seq(-10, 10, by = 0.25), 
        function(r) 
          abs(
            c(
              mean_generalized(x, w, r) - 
                if (r != 0) (stats::weighted.mean(x^r, w))^(1 / r) else exp(weighted.mean(log(x), w)),
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
        function(r) mean_generalized(x, w, r - runif(1, 0, 5)) <= mean_generalized(x, w, r),
        logical(1)
      )
    )
    # Checks for NA and length-0 inputs
    is.na(mean_arithmetic(NA))
    is.na(mean_arithmetic(NA, 1))
    is.na(mean_arithmetic(NA, 0.5, scale = FALSE))
    is.na(mean_arithmetic(1, NA))
    is.na(mean_arithmetic(NaN))
    is.na(mean_arithmetic(1, NaN))
    is.nan(mean_arithmetic(NA, na.rm = TRUE))
    is.nan(mean_arithmetic(NaN, na.rm = TRUE))
    is.nan(mean_arithmetic(NA, 1, na.rm = TRUE))
    is.nan(mean_arithmetic(NA, 0.5, na.rm = TRUE, scale = FALSE))
    is.nan(mean_arithmetic(1, NA, na.rm = TRUE))
    is.nan(mean_arithmetic(1, NaN, na.rm = TRUE))
    is.nan(mean_arithmetic(numeric(0)))
    is.nan(mean_arithmetic(numeric(0), numeric(0)))
    abs(mean_arithmetic(c(1, NA), na.rm = TRUE) - 1) < .Machine$double.eps^0.5
    abs(mean_arithmetic(1:2, c(2, NA), na.rm = TRUE) - 1) < .Machine$double.eps^0.5
    abs(mean_arithmetic(1:2, c(2, NA), na.rm = TRUE, scale = FALSE) - 2) < .Machine$double.eps^0.5
    is.na(mean_geometric(NA))
    is.na(mean_harmonic(NA))
    is.nan(mean_geometric(NA, na.rm = TRUE))
    is.nan(mean_harmonic(NA, na.rm = TRUE))
    # Change weights
    all(
      apply(
        expand.grid(a = seq(-3, 3, by = 0.25), b = seq(-3, 3, by = 0.25)), 
        1,
        function(p) {
          c(
            abs(mean_generalized(x, w, p[1]) - 
                  mean_generalized(x, weights_change(x, w, p[1], p[2]), p[2])) < .Machine$double.eps^0.5,
            abs(mean_generalized(x, r = p[1]) - 
                  mean_generalized(x, weights_change(x, r = p[1], k = p[2]), p[2])) < .Machine$double.eps^0.5
          )
        }
      )
    )
    abs(mean_geometric(c(2, sqrt(2)^2)) - 
          mean_arithmetic(c(2, sqrt(2)^2), weights_g2a(c(2, sqrt(2)^2)))) < .Machine$double.eps^0.5
    # Change weights with NAs
    all(
      apply(
        expand.grid(a = seq(-2, 2, by = 0.5), b = seq(-2, 2, by = 0.5)), 
        1,
        function(p) {
          abs(mean_generalized(xna, w, p[1], na.rm = TRUE) - 
                mean_generalized(xna, weights_change(xna, w, p[1], p[2], na.rm = TRUE), p[2], na.rm = TRUE)) < .Machine$double.eps^0.5
        }
      )
    )
    # Factor weights
    all(weights_factor(x, w, 0) == w)
    all(
      vapply(
        seq(-5, 5, by = 0.25), 
        function(r) 
          c(
            abs(mean_generalized(x * a, w, r) - 
                  mean_generalized(x, w, r) * mean_generalized(a, weights_factor(x, w, r), r)) < .Machine$double.eps^0.5,
            abs(mean_generalized(x * a, r = r) - 
                  mean_generalized(x, r = r) * mean_generalized(a, weights_factor(x, r = r), r)) < .Machine$double.eps^0.5
          ),
        logical(2)
      )
    )
  },
  local = getNamespace("gpindex")
)

#---- Tests for generalized log means ----

stopifnot(
  exprs = {
    # Checks against known values
    logmean(1, 1) == 1
    logmean(1, 0) == 0
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
                  ((b^r - a^r)/(r * (b - a)))^(1 / (r - 1))
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
              logmean_generalized(a, b, r) - logmean_generalized(b, a, r)) < .Machine$double.eps^0.5
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
    is.nan(logmean(NaN, NA_real_))
    is.nan(logmean(1, NaN))
    is.nan(logmean(NaN, 1))
    is.nan(logmean(NaN, NaN))
    # Test of a == b
    logmean(0.00001, 1 / 100000) == 1 / 100000
    logmean(2, sqrt(2)^2) == 2
    logmean_generalized(2, sqrt(2)^2, 0.9) == 2
    # Test of recycling
    logmean(1, 1:5) == logmean(c(1, 1, 1, 1, 1), 1:5)
  },
  local = getNamespace("gpindex")
)