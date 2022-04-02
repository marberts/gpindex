library(gpindex)

set.seed(4321)

#---- Tests for offsetting prices ----
# Make some data for the tests
id <- letters[c(5:1, 1:5)]
period <- rep(1:2, each = 5)

# Simple cases
back_period(NULL, NULL)
back_period(1:4)
base_period(1:4)
# Attributes shouldn't do anything
back_period(matrix(1:4))
# Change time periods
back_period(factor(rep(1, 4), levels = 0:1), 1:4)
back_period(factor(1:4, levels = 4:1))
# A more interesting case
back_period(period, id)
back_period(replace(period, 2, NA), id)
back_period(period[-1], id[-1])
back_period(period, replace(id, 1:2, NA)) # NA products shouldn't trigger a warning

#---- Tests for outliers ----
x <- log(runif(1e5, 0.1, 10))

all.equal(fixed_cutoff(x), x > 2.5 | x < 1 / 2.5)
all.equal(quartile_method(x), x > median(x) + (quantile(x, 0.75) - quantile(x, 0.5)) * 2.5 |
            x < median(x) - (quantile(x, 0.5) - quantile(x, 0.25)) * 2.5)
all.equal(quartile_method(x, a = c(0, 1)), x > median(x) + c((quantile(x, 0.75) - quantile(x, 0.5)), median(x)) * 2.5 |
            x < median(x) - c((quantile(x, 0.5) - quantile(x, 0.25)), median(x)) * 2.5)
all.equal(resistant_fences(x), x > quantile(x, 0.75) + (quantile(x, 0.75) - quantile(x, 0.25)) * 2.5 |
            x < quantile(x, 0.25) - (quantile(x, 0.75) - quantile(x, 0.25)) * 2.5)
sum(resistant_fences(x)) <= sum(quartile_method(x))
all.equal(robust_z(x), abs(x - median(x)) / mad(x) > 2.5)

x <- seq(0.1, 2, by = 0.2)

all.equal(hb_transform(x), ifelse(x < median(x), 1 - median(x) / x, x / median(x) - 1))
hb_transform(x - 1)

all.equal(tukey_algorithm(integer(0)), logical(0))
all.equal(tukey_algorithm(2), FALSE)
all.equal(tukey_algorithm(x), c(TRUE, rep(FALSE, 8), TRUE))
all.equal(tukey_algorithm(c(NA, 1, 2, 3)), c(NA, T, F, T))
