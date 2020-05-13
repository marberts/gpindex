# Some data for tests
x <- rlnorm(15)
xna <- rlnorm(15)
xna[sample(15, 4)] <- NA
w <- runif(15)
a <- runif(15)
b <- rlnorm(15)

# Tests for generalized means
stopifnot(
  exprs = {
    # Simple checks against known values
    abs(arithmetic_mean(1:5) - 3) < .Machine$double.eps^0.5
    abs(arithmetic_mean(1:4, 4:1) - 2) < .Machine$double.eps^0.5
    abs(arithmetic_mean(1:4, scale = FALSE) - 10) < .Machine$double.eps^0.5
    abs(arithmetic_mean(1:4, 4:1, scale = FALSE) - 20) < .Machine$double.eps^0.5
    abs(geometric_mean(c(1, 4)) - 2) < .Machine$double.eps^0.5
    abs(geometric_mean(1:3, 1:3) - prod((1:3)^(1:3 / 6))) < .Machine$double.eps^0.5
    abs(harmonic_mean(1:2) - 4 / 3) < .Machine$double.eps^0.5
    abs(harmonic_mean(1:3, 1:3) - 2) < .Machine$double.eps^0.5
    # Is the fundamental inequality satisfied?
    geometric_mean(1:100, 100:1) <= arithmetic_mean(1:100, 100:1)
    geometric_mean(1:100, 100:1) >= harmonic_mean(1:100, 100:1)
    # Tests against a simple implementation with stats::weighted.mean
    all(
      vapply(
        seq(-10, 10, by = 0.25), 
        function(r) 
          abs(
            c(
              generalized_mean(x, w, r) - 
                if (r != 0) (stats::weighted.mean(x^r, w))^(1 / r) else exp(weighted.mean(log(x), w)),
              generalized_mean(x, r = r) - 
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
        function(r) generalized_mean(x, w, r - runif(1, 0, 5)) <= generalized_mean(x, w, r),
        logical(1)
      )
    )
    # Checks for NA and length-0 inputs
    is.na(arithmetic_mean(NA))
    is.na(arithmetic_mean(NA, 1))
    is.na(arithmetic_mean(NA, 0.5, scale = FALSE))
    is.na(arithmetic_mean(1, NA))
    is.nan(arithmetic_mean(NaN))
    is.na(arithmetic_mean(1, NaN))
    is.nan(arithmetic_mean(NA, na.rm = TRUE))
    is.nan(arithmetic_mean(NaN, na.rm = TRUE))
    is.nan(arithmetic_mean(NA, 1, na.rm = TRUE))
    is.nan(arithmetic_mean(NA, 0.5, na.rm = TRUE, scale = FALSE))
    is.nan(arithmetic_mean(1, NA, na.rm = TRUE))
    is.nan(arithmetic_mean(1, NaN, na.rm = TRUE))
    is.nan(arithmetic_mean(numeric(0)))
    is.nan(arithmetic_mean(numeric(0), numeric(0)))
    abs(arithmetic_mean(c(1, NA), na.rm = TRUE) - 1) < .Machine$double.eps^0.5
    abs(arithmetic_mean(1:2, c(2, NA), na.rm = TRUE) - 1) < .Machine$double.eps^0.5
    abs(arithmetic_mean(1:2, c(2, NA), na.rm = TRUE, scale = FALSE) - 2) < .Machine$double.eps^0.5
    is.na(geometric_mean(NA))
    is.na(harmonic_mean(NA))
    is.nan(geometric_mean(NA, na.rm = TRUE))
    is.nan(harmonic_mean(NA, na.rm = TRUE))
    # Change weights
    all(
      apply(
        expand.grid(a = seq(-3, 3, by = 0.25), b = seq(-3, 3, by = 0.25)), 
        1,
        function(p) {
          c(
            abs(generalized_mean(x, w, p[1]) - 
                  generalized_mean(x, change_weights(x, w, p[1], p[2]), p[2])) < .Machine$double.eps^0.5,
            abs(generalized_mean(x, r = p[1]) - 
                  generalized_mean(x, change_weights(x, r = p[1], k = p[2]), p[2])) < .Machine$double.eps^0.5
          )
        }
      )
    )
    abs(geometric_mean(c(2, sqrt(2)^2)) - 
          arithmetic_mean(c(2, sqrt(2)^2), geometric_to_arithmetic(c(2, sqrt(2)^2)))) < .Machine$double.eps^0.5
    # Change weights with NAs
    all(
      apply(
        expand.grid(a = seq(-2, 2, by = 0.5), b = seq(-2, 2, by = 0.5)), 
        1,
        function(p) {
          abs(generalized_mean(xna, w, p[1], na.rm = TRUE) - 
                generalized_mean(xna, change_weights(xna, w, p[1], p[2], na.rm = TRUE), p[2], na.rm = TRUE)) < .Machine$double.eps^0.5
        }
      )
    )
    # Factor weights
    all(factor_weights(x, w, 0) == w)
    all(
      vapply(
        seq(-10, 10, by = 0.25), 
        function(r) 
          c(
            abs(generalized_mean(x * a, w, r) - 
                  generalized_mean(x, w, r) * generalized_mean(a, factor_weights(x, w, r), r)) < .Machine$double.eps^0.5,
            abs(generalized_mean(x * a, r = r) - 
                  generalized_mean(x, r = r) * generalized_mean(a, factor_weights(x, r = r), r)) < .Machine$double.eps^0.5
          ),
        logical(2)
      )
    )
  },
  local = getNamespace("gpindex")
)

# Tests for generalized log means

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
              generalized_logmean(a, b, r) - 
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
    # Checks for NA and length-0 inputs
    identical(logmean(numeric(0), numeric(0)), numeric(0))
    is.na(logmean(1, NA_real_))
    is.nan(logmean(1, NaN))
    # Test of a == b
    logmean(0.00001, 1 / 100000) == 1 / 100000
    logmean(2, sqrt(2)^2) == 2
    # Test of recycling
    logmean(1, 1:5) == logmean(c(1, 1, 1, 1, 1), 1:5)
  },
  local = getNamespace("gpindex")
)