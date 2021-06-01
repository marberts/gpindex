library(gpindex)

# Some data for tests
set.seed(4321)
x <- rnorm(15)^2
xna <- replace(x, 2, NA)
w <- runif(15, 0, 2)

#---- Tests for weights_transmute ----
all.equal(weights_transmute(2, 2)(x), rep(1, length(x)))
all.equal(weights_transmute(0, 0)(xna, w), replace(w, 2, NA))
all.equal(weights_transmute(1, 1)(c(1, NA)), c(1, NA))
all.equal(weights_transmute(2, 1)(c(1, NA)), c(1, NA))
all.equal(weights_transmute(7, -3)(x, weights_transmute(-3, 7)(x, w)), w)

#---- Tests for contributions ----
all.equal(contributions_arithmetic(1:4), c(0, 0.25, 0.5, 0.75))
all.equal(contributions_harmonic(1:4), c(0, 0.24, 0.32, 0.36))
all.equal(contributions_geometric(c(1, 4)), c(0, 1))
all.equal(sum(contributions(-3.75)(x, w)), mean_generalized(-3.75)(x, w) - 1)
all.equal(sum(contributions(3.75)(xna, w), na.rm = TRUE), mean_generalized(3.75)(xna, w, na.rm = TRUE) - 1)
    
#---- Tests for weights_factor ----
all.equal(weights_factor(0)(c(1, NA)), c(1, NA))
all.equal(weights_factor(0)(x), rep(1, length(x)))
all.equal(weights_factor(0)(x, w), w)
all.equal(weights_update(xna, w), xna * w)

#---- Tests for weights_scale ----
all.equal(sum(weights_scale(w)), 1)
all.equal(weights_scale(c(1:2, NA)), c(1:2, NA) / 3)

#---- Tests for contributions_nested ----
all.equal(sum(contributions_nested(3, c(-1, 2), c(0.75, 0.25))(x)),
          mean_generalized(3)(c(mean_harmonic(x), mean_generalized(2)(x)), c(0.75, 0.25)) - 1)

all.equal(sum(contributions_nested2(3, c(-1, 2), c(0.75, 0.25))(x)),
          mean_generalized(3)(c(mean_harmonic(x), mean_generalized(2)(x)), c(0.75, 0.25)) - 1)

all.equal(sum(contributions_nested(0, c(1, -1), c(0.5, 0.5))(x)),
          prod(sqrt(c(mean_harmonic(x), mean_arithmetic(x)))) - 1)

all.equal(contributions_nested(1, c(0, -1), c(1, 2))(xna, x, w),
          contributions_nested2(1, c(0, -1), c(1, 2))(xna, x, w))

all.equal(sum(contributions_nested(1, c(0, -1), c(1, 2))(xna, x, w), na.rm = TRUE),
          mean_nested(1, c(0, -1), c(1, 2))(xna, x, w, na.rm = TRUE) - 1)

all.equal(sum(contributions_nested(0, c(3, -2))(xna, w, xna), na.rm = TRUE),
          mean_nested(0, c(3, -2))(xna, w, xna, na.rm = TRUE) - 1)

all.equal(sum(contributions_nested2(0, c(3, -2))(xna, w, xna), na.rm = TRUE),
          mean_nested(0, c(3, -2))(xna, w, xna, na.rm = TRUE) - 1)
