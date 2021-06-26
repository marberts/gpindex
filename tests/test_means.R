library(gpindex)

# Some data for tests
set.seed(1234)
x <- rlnorm(15)
xna <- replace(rlnorm(15), 4, NA)
w <- runif(15)
a <- runif(15, 0, 5)
b <- rlnorm(15)

#---- Tests for Pythagorean means ----
# Known values
all.equal(arithmetic_mean(1:5), 3)
all.equal(arithmetic_mean(1:4, 4:1), 2)
all.equal(geometric_mean(c(1, 4)), 2)
all.equal(geometric_mean(1:3, 1:3), prod((1:3)^(1:3 / 6)))
all.equal(harmonic_mean(1:2), 4 / 3)
all.equal(harmonic_mean(1:3, 1:3), 2)
# Base implementation
all.equal(arithmetic_mean(x), mean(x))
all.equal(arithmetic_mean(x, w), weighted.mean(x, w))
all.equal(geometric_mean(x), exp(mean(log(x))))
all.equal(geometric_mean(x, w), prod(x^(w / sum(w))))
all.equal(harmonic_mean(x), 1 / mean(1 / x))
all.equal(harmonic_mean(x, w), 1 / weighted.mean(1 / x, w))
# NAs
all.equal(arithmetic_mean(c(1, NA), na.rm = TRUE), 1)
all.equal(arithmetic_mean(1:2, c(2, NA), na.rm = TRUE), 1)
all.equal(arithmetic_mean(c(1, NA), c(1, 2), na.rm = TRUE), 1)
# Zero values
all.equal(geometric_mean(replace(x, 3, 0), w), 0)
all.equal(harmonic_mean(replace(x, 3, 0), w), 0)
all.equal(geometric_mean(c(1, 1, 2), c(0.5, 0, 0.5)), sqrt(2))
all.equal(harmonic_mean(c(1, -1, 2), c(0.5, 0, 0.5)), 4/3)

#---- Tests for generalized means ----
# Base implementation
all.equal(generalized_mean(-2.5)(x), mean(x^(-2.5))^(1 / -2.5))
all.equal(generalized_mean(-0.1)(x, w), weighted.mean(x^(-0.1), w)^(1 / -0.1))
all.equal(generalized_mean(3.25)(x, w), weighted.mean(x^(3.25), w)^(1 / 3.25))
all.equal(generalized_mean(9.9)(x), mean(x^(9.9))^(1 / 9.9))
# Reversal
all.equal(generalized_mean(-3)(x, w), 1 / generalized_mean(3)(1 / x, w))
all.equal(generalized_mean(2.3)(x, w), 1 / generalized_mean(-2.3)(1 / x, w))
# General inequality
generalized_mean(-2.7)(x, w) < generalized_mean(-2)(x, w)
generalized_mean(0.6)(x, w) > generalized_mean(-2)(x, w)
generalized_mean(4.6)(x, w) > generalized_mean(0.6)(x, w)
# Change weights
all.equal(generalized_mean(-4.4)(x, w),
          generalized_mean(0)(x, transmute_weights(-4.4, 0)(x, w)))
all.equal(generalized_mean(3.8)(x, w),
          generalized_mean(-1)(x, transmute_weights(3.8, -1)(x, w)))
all.equal(generalized_mean(1)(xna, w, na.rm = TRUE),
          generalized_mean(-0.04)(xna, transmute_weights(1, -0.04)(xna, w), na.rm = TRUE))
all.equal(generalized_mean(0)(xna, w, na.rm = TRUE),
          generalized_mean(0)(xna, transmute_weights(0, 0)(xna, w), na.rm = TRUE))
# Factor weights
all.equal(generalized_mean(-0.9)(x * a, w),
          generalized_mean(-0.9)(x, w) * generalized_mean(-0.9)(a, factor_weights(-0.9)(x, w)))
all.equal(generalized_mean(6)(x * a, w),
          generalized_mean(6)(x, w) * generalized_mean(6)(a, factor_weights(6)(x, w)))
all.equal(generalized_mean(2.2)(xna * a, w, na.rm = TRUE),
          generalized_mean(2.2)(xna, w, na.rm = TRUE) * generalized_mean(2.2)(a, factor_weights(2.2)(xna, w), na.rm = TRUE))
all.equal(generalized_mean(0)(xna * a, w, na.rm = TRUE),
          generalized_mean(0)(xna, w, na.rm = TRUE) * generalized_mean(0)(a, factor_weights(0)(xna, w), na.rm = TRUE))
all.equal(generalized_mean(1)(x * a, w),
          generalized_mean(1)(x, update_weights(a, w)) * generalized_mean(1)(a, w))
# Errors and warnings
try(generalized_mean("a"))
try(generalized_mean(1:2))
try(generalized_mean(NaN))
generalized_mean(1e-9)(1)
try(geometric_mean(1:5, 1:3))

#---- Tests for generalized log means ----
# Known values
all.equal(logmean(1, 1), 1)
all.equal(logmean(1, 0), 0)
all.equal(logmean(2, 1), 1 / log(2))
all.equal(generalized_logmean(2)(1, 2), 1.5)
all.equal(generalized_logmean(-1)(1, 2), sqrt(2))
all.equal(extended_mean(0, 0)(4, 4), 4)
# Symmetry
all.equal(logmean(a, b), logmean(b, a))
all.equal(extended_mean(-0.1, 2.5)(a, b), extended_mean(2.5, -0.1)(b, a))
# Tolerance
all.equal(logmean(2, sqrt(2)^2), 2)
all.equal(generalized_logmean(0.9)(2, sqrt(2)^2), 2)
all.equal(generalized_logmean(1.1)(2, sqrt(2)^2), 2)
# Recycling
all(logmean(2:3, rep(sqrt(2)^2, 5)) == c(2, logmean(3, sqrt(2)^2), 2, logmean(3, sqrt(2)^2), 2))
# Identities
all.equal(generalized_logmean(-1)(a, b),
          apply(matrix(c(a, b), ncol = 2), 1, geometric_mean))
all.equal(generalized_logmean(2)(a, b),
          apply(matrix(c(a, b), ncol = 2), 1, arithmetic_mean))
all.equal(generalized_logmean(-2)(a, b),
          apply(matrix(c(a, b), ncol = 2), 1, function(x) (harmonic_mean(x) * geometric_mean(x)^2)^(1 / 3)))
all.equal(generalized_logmean(0.5)(a, b),
          apply(matrix(c(a, b), ncol = 2), 1, function(x) (arithmetic_mean(x) + geometric_mean(x)) / 2))
all.equal(logmean(a, b),
          apply(matrix(c(a, b), ncol = 2), 1, geometric_mean)^2 * logmean(1 / a, 1 / b))
all.equal(extended_mean(-2, -1)(a, b), 
          apply(matrix(c(a, b), ncol = 2), 1, harmonic_mean))
all.equal(extended_mean(-2, 2)(a, b), 
          apply(matrix(c(a, b), ncol = 2), 1, geometric_mean))
all.equal(extended_mean(3.5, -3.5)(a, b), 
          apply(matrix(c(a, b), ncol = 2), 1, geometric_mean))
all.equal(extended_mean(2, 2)(a, b), (a^a^2 / b^b^2)^(1 / (a^2 - b^2)) / exp(1)^(1 / 2))
# Errors and warnings
try(extended_mean(1, NA))
try(extended_mean(NA, 1))
try(extended_mean("a", mtcars))
extended_mean(1e-9, 1)(1, 2)
extended_mean(1, 1e-9)(1, 2)
extended_mean(1, 1 + 1e-9)(1, 2)

#---- Tests for Lehmer means ----
# Check Pythagorean means
all.equal(lehmer_mean(1)(x, w), weighted.mean(x, w))
all.equal(lehmer_mean(0)(x, w), 1 / weighted.mean(1 / x, w))
all.equal(lehmer_mean(0.5)(x[1:2]), sqrt(prod(x[1:2])))
# Reversal
all.equal(lehmer_mean(5)(x, w), 1 / lehmer_mean(-4)(1 / x, w))
all.equal(lehmer_mean(-3)(x), 1 / lehmer_mean(4)(1 / x))
all.equal(contraharmonic_mean(xna, w, na.rm = TRUE), 1 / lehmer_mean(-1)(1 / xna, w, na.rm = TRUE))

#---- Tests for %^% ----
all.equal(mapply(gpindex:::`%^%`, list(x), seq(-2, 2, by = 0.5)[-5]),
          mapply(`^`, list(x), seq(-2, 2, by = 0.5)[-5]))
all.equal(gpindex:::`%^%`(x, 0), 1)

#---- Tests for nested means ----
all.equal(nested_mean(-3, c(2, 0.3), 3:4)(xna, a, b, na.rm = TRUE),
          generalized_mean(-3)(c(generalized_mean(2)(xna, a, na.rm = TRUE),
                                 generalized_mean(0.3)(xna, b, na.rm = TRUE)),
                               3:4))

all.equal(nested_mean(2, c(1, 0), c(2, 0))(x, a, b), arithmetic_mean(x, a))

all.equal(nested_mean(1, c(1, 1))(x, a, a), arithmetic_mean(x, a))

is.na(fisher_mean(1, NA, 1))

fisher_mean(1, NA, 1, na.rm = TRUE)
