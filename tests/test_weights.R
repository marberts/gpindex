library(gpindex)

# Some data for tests
set.seed(4321)
x <- rnorm(15)^2
xna <- replace(x, 2, NA)
w <- runif(15, 0, 2)
f <- factor(sample(letters[1:3], 15, TRUE))

#---- Tests for transmute_weights ----
all.equal(transmute_weights(2, 2)(x), rep(1, length(x)))
all.equal(transmute_weights(0, 0)(xna, w), replace(w, 2, NA))
all.equal(transmute_weights(1, 1)(c(1, NA)), c(1, NA))
all.equal(transmute_weights(2, 1)(c(1, NA)), c(1, NA))
all.equal(transmute_weights(7, -3)(x, transmute_weights(-3, 7)(x, w)), w)
all.equal(grouped(transmute_weights(1, 2))(x, w, group = f),
          unsplit(Map(transmute_weights(1, 2), split(x, f), split(w, f)), f))

#---- Tests for contributions ----
all.equal(arithmetic_contributions(1:4), c(0, 0.25, 0.5, 0.75))
all.equal(harmonic_contributions(1:4), c(0, 0.24, 0.32, 0.36))
all.equal(geometric_contributions(c(1, 4)), c(0, 1))
all.equal(sum(contributions(-3.75)(x, w)), generalized_mean(-3.75)(x, w) - 1)
all.equal(sum(contributions(3.75)(xna, w), na.rm = TRUE), generalized_mean(3.75)(xna, w, na.rm = TRUE) - 1)
all.equal(as.numeric(tapply(grouped(geometric_contributions)(x, group = f), f, sum)),
          as.numeric(tapply(x, f, geometric_mean) - 1))
    
#---- Tests for factor_weights ----
all.equal(factor_weights(0)(c(1, NA)), c(1, NA))
all.equal(factor_weights(0)(x), rep(1, length(x)))
all.equal(factor_weights(0)(x, w), w)
all.equal(update_weights(xna, w), xna * w)
all.equal(grouped(update_weights)(x, w, group = f), x * w)

#---- Tests for scale_weights ----
all.equal(sum(scale_weights(w)), 1)
all.equal(scale_weights(c(1:2, NA)), c(1:2, NA) / 3)

#---- Tests for nested_contributions ----
all.equal(sum(nested_contributions(3, c(-1, 2), c(0.75, 0.25))(x)),
          generalized_mean(3)(c(harmonic_mean(x), generalized_mean(2)(x)), c(0.75, 0.25)) - 1)

all.equal(sum(nested_contributions2(3, c(-1, 2), c(0.75, 0.25))(x)),
          generalized_mean(3)(c(harmonic_mean(x), generalized_mean(2)(x)), c(0.75, 0.25)) - 1)

all.equal(sum(nested_contributions(0, c(1, -1), c(0.5, 0.5))(x)),
          prod(sqrt(c(harmonic_mean(x), arithmetic_mean(x)))) - 1)

all.equal(nested_contributions(1, c(0, -1), c(1, 2))(xna, x, w),
          nested_contributions2(1, c(0, -1), c(1, 2))(xna, x, w))

all.equal(sum(nested_contributions(1, c(0, -1), c(1, 2))(xna, x, w), na.rm = TRUE),
          nested_mean(1, c(0, -1), c(1, 2))(xna, x, w, na.rm = TRUE) - 1)

all.equal(sum(nested_contributions(0, c(3, -2))(xna, w, xna), na.rm = TRUE),
          nested_mean(0, c(3, -2))(xna, w, xna, na.rm = TRUE) - 1)

all.equal(sum(nested_contributions2(0, c(3, -2))(xna, w, xna), na.rm = TRUE),
          nested_mean(0, c(3, -2))(xna, w, xna, na.rm = TRUE) - 1)

# TODO: Is this correct? I think it only happens when one set of weights is all NA
all.equal(fisher_contributions(1:2, c(NA, NA)), c(0, 1 / 3))
all.equal(fisher_contributions2(1:2, c(NA, NA)), c(NaN, NaN))
