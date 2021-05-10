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
all.equal(mean_arithmetic(1:5), 3)
all.equal(mean_arithmetic(1:4, 4:1), 2)
all.equal(mean_arithmetic(1:4, scale = FALSE), 10)
all.equal(mean_arithmetic(1:4, 4:1, scale = FALSE), 20)
all.equal(mean_geometric(c(1, 4)), 2)
all.equal(mean_geometric(1:3, 1:3), prod((1:3)^(1:3 / 6)))
all.equal(mean_harmonic(1:2), 4 / 3)
all.equal(mean_harmonic(1:3, 1:3), 2)
# Base implementation
all.equal(mean_arithmetic(x), mean(x))
all.equal(mean_arithmetic(x, w), weighted.mean(x, w))
all.equal(mean_geometric(x), exp(mean(log(x))))
all.equal(mean_geometric(x, w), prod(x^(w / sum(w))))
all.equal(mean_harmonic(x), 1 / mean(1 / x))
all.equal(mean_harmonic(x, w), 1 / weighted.mean(1 / x, w))
# NAs
all.equal(mean_arithmetic(NA, 0.5, na.rm = TRUE, scale = FALSE), 0)
all.equal(mean_arithmetic(c(1, NA), na.rm = TRUE), 1)
all.equal(mean_arithmetic(1:2, c(2, NA), na.rm = TRUE), 1)
all.equal(mean_arithmetic(c(1, NA), c(1, 2), na.rm = TRUE), 1)
all.equal(mean_arithmetic(1:2, c(2, NA), na.rm = TRUE, scale = FALSE), 2)
all.equal(mean_arithmetic(c(1, NA), c(NA, 2), na.rm = TRUE, scale = FALSE), 0)
# Zero values
all.equal(mean_geometric(replace(x, 3, 0), w), 0)
all.equal(mean_harmonic(replace(x, 3, 0), w), 0)
all.equal(mean_geometric(c(1, 1, 2), c(0.5, 0, 0.5)), sqrt(2))
all.equal(mean_harmonic(c(1, -1, 2), c(0.5, 0, 0.5)), 4/3)

#---- Tests for generalized means ----
# Base implementation
all.equal(mean_generalized(-2.5)(x), mean(x^(-2.5))^(1 / -2.5))
all.equal(mean_generalized(-0.1)(x, w), weighted.mean(x^(-0.1), w)^(1 / -0.1))
all.equal(mean_generalized(3.25)(x, w), weighted.mean(x^(3.25), w)^(1 / 3.25))
all.equal(mean_generalized(9.9)(x), mean(x^(9.9))^(1 / 9.9))
# Reversal
all.equal(mean_generalized(-3)(x, w), 1 / mean_generalized(3)(1 / x, w))
all.equal(mean_generalized(2.3)(x, w), 1 / mean_generalized(-2.3)(1 / x, w))
# General inequality
mean_generalized(-2.7)(x, w) < mean_generalized(-2)(x, w)
mean_generalized(0.6)(x, w) > mean_generalized(-2)(x, w)
mean_generalized(4.6)(x, w) > mean_generalized(0.6)(x, w)
# Change weights
all.equal(mean_generalized(-4.4)(x, w),
          mean_generalized(0)(x, weights_transmute(-4.4, 0)(x, w)))
all.equal(mean_generalized(3.8)(x, w),
          mean_generalized(-1)(x, weights_transmute(3.8, -1)(x, w)))
all.equal(mean_generalized(1)(xna, w, na.rm = TRUE),
          mean_generalized(-0.04)(xna, weights_transmute(1, -0.04)(xna, w), na.rm = TRUE))
all.equal(mean_generalized(0)(xna, w, na.rm = TRUE),
          mean_generalized(0)(xna, weights_transmute(0, 0)(xna, w), na.rm = TRUE))
# Factor weights
all.equal(mean_generalized(-0.9)(x * a, w),
          mean_generalized(-0.9)(x, w) * mean_generalized(-0.9)(a, weights_factor(-0.9)(x, w)))
all.equal(mean_generalized(6)(x * a, w),
          mean_generalized(6)(x, w) * mean_generalized(6)(a, weights_factor(6)(x, w)))
all.equal(mean_generalized(2.2)(xna * a, w, na.rm = TRUE),
          mean_generalized(2.2)(xna, w, na.rm = TRUE) * mean_generalized(2.2)(a, weights_factor(2.2)(xna, w), na.rm = TRUE))
all.equal(mean_generalized(0)(xna * a, w, na.rm = TRUE),
          mean_generalized(0)(xna, w, na.rm = TRUE) * mean_generalized(0)(a, weights_factor(0)(xna, w), na.rm = TRUE))
all.equal(mean_generalized(1)(x * a, w),
          mean_generalized(1)(x, weights_update(a, w)) * mean_generalized(1)(a, w))

#---- Tests for generalized log means ----
# Known values
all.equal(logmean(1, 1), 1)
all.equal(logmean(1, 0), 0)
all.equal(logmean(2, 1), 1 / log(2))
all.equal(logmean_generalized(2)(1, 2), 1.5)
all.equal(logmean_generalized(-1)(1, 2), sqrt(2))
all.equal(mean_extended(0, 0)(4, 4), 4)
# Symmetry
all.equal(logmean(a, b), logmean(b, a))
all.equal(mean_extended(-0.1, 2.5)(a, b), mean_extended(2.5, -0.1)(b, a))
# Tolerance
all.equal(logmean(2, sqrt(2)^2), 2)
all.equal(logmean_generalized(0.9)(2, sqrt(2)^2), 2)
all.equal(logmean_generalized(1.1)(2, sqrt(2)^2), 2)
# Recycling
all(logmean(2:3, rep(sqrt(2)^2, 5)) == c(2, logmean(3, sqrt(2)^2), 2, logmean(3, sqrt(2)^2), 2))
# Identities
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
all.equal(mean_extended(-2, -1)(a, b), 
          apply(matrix(c(a, b), ncol = 2), 1, mean_harmonic))
all.equal(mean_extended(-2, 2)(a, b), 
          apply(matrix(c(a, b), ncol = 2), 1, mean_geometric))
all.equal(mean_extended(3.5, -3.5)(a, b), 
          apply(matrix(c(a, b), ncol = 2), 1, mean_geometric))
all.equal(mean_extended(2, 2)(a, b), (a^a^2 / b^b^2)^(1 / (a^2 - b^2)) / exp(1)^(1 / 2))

#---- Tests for Lehmer means ----
# Check Pythagorean means
all.equal(mean_lehmer(1)(x, w), weighted.mean(x, w))
all.equal(mean_lehmer(0)(x, w), 1 / weighted.mean(1 / x, w))
all.equal(mean_lehmer(0.5)(x[1:2]), sqrt(prod(x[1:2])))
# Reversal
all.equal(mean_lehmer(5)(x, w), 1 / mean_lehmer(-4)(1 / x, w))
all.equal(mean_lehmer(-3)(x), 1 / mean_lehmer(4)(1 / x))
all.equal(mean_contraharmonic(xna, w, na.rm = TRUE), 1 / mean_lehmer(-1)(1 / xna, w, na.rm = TRUE))

#---- Tests for %^% ----
all.equal(mapply(gpindex:::`%^%`, list(x), seq(-2, 2, by = 0.5)[-5]),
          mapply(`^`, list(x), seq(-2, 2, by = 0.5)[-5]))
all.equal(gpindex:::`%^%`(x, 0), 1)
