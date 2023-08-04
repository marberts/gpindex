# Some data for tests
set.seed(1234)
x <- rlnorm(15)
xna <- replace(rlnorm(15), 4, NA)
w <- runif(15)
wna <- replace(runif(15), 7, NA)
a <- runif(15, 0, 5)
b <- rlnorm(15)

test_that("Pythagorean means agree with known values", {
  expect_equal(arithmetic_mean(1:5), 3)
  expect_equal(arithmetic_mean(1:4, 4:1), 2)
  expect_equal(geometric_mean(c(1, 4)), 2)
  expect_equal(geometric_mean(1:3, 1:3), prod((1:3)^(1:3 / 6)))
  expect_equal(harmonic_mean(1:2), 4 / 3)
  expect_equal(harmonic_mean(1:3, 1:3), 2)
  # NAs
  expect_equal(arithmetic_mean(c(1, NA), na.rm = TRUE), 1)
  expect_equal(arithmetic_mean(1:2, c(2, NA), na.rm = TRUE), 1)
  expect_equal(arithmetic_mean(c(1, NA), c(1, 2), na.rm = TRUE), 1)
  # Zero values
  expect_equal(arithmetic_mean(0:5, 1:6), 3 + 1 / 3)
  expect_equal(geometric_mean(0:5, 1:6), 0)
  expect_equal(harmonic_mean(0:5, 1:6), 0)
  expect_equal(arithmetic_mean(c(1, 1, 2), c(0.5, 0, 0.5)), 1.5)
  expect_equal(geometric_mean(c(1, 1, 2), c(0.5, 0, 0.5)), sqrt(2))
  expect_equal(harmonic_mean(c(1, -1, 2), c(0.5, 0, 0.5)), 4 / 3)
  # Base implementation
  expect_equal(arithmetic_mean(x, w), weighted.mean(x, w))
  expect_equal(geometric_mean(x, w), exp(weighted.mean(log(x), w)))
  expect_equal(harmonic_mean(x, w), 1 / (weighted.mean(1 / x, w)))
})

test_that("generalized means agree with base implementation", {
  expect_equal(generalized_mean(-2.5)(x),
               mean(x^(-2.5))^(1 / -2.5))
  expect_equal(generalized_mean(0.6)(xna, na.rm = TRUE),
               mean(xna^(0.6), na.rm = TRUE)^(1 / 0.6))
  expect_equal(generalized_mean(-0.1)(x, w),
               weighted.mean(x^(-0.1), w)^(1 / -0.1))
  expect_equal(generalized_mean(3.25)(xna, w, na.rm = TRUE),
               weighted.mean(xna^(3.25), w, na.rm = TRUE)^(1 / 3.25))
})

test_that("generalized means satifies key properties", {
  # Reversal
  expect_equal(generalized_mean(-3)(x),
               1 / generalized_mean(3)(1 / x))
  expect_equal(generalized_mean(2.3)(x, w),
               1 / generalized_mean(-2.3)(1 / x, w))
  # General inequality
  generalized_mean(-2.7)(x, w) < generalized_mean(-2)(x, w)
  generalized_mean(0.6)(x, w) > generalized_mean(-2)(x, w)
  generalized_mean(4.6)(x, w) > generalized_mean(0.6)(x, w)
})

test_that("generalized means work with transmuted weights", {
  expect_equal(generalized_mean(-4.4)(x),
               generalized_mean(0)(x, transmute_weights(-4.4, 0)(x)))
  expect_equal(generalized_mean(3.8)(x, w),
               generalized_mean(-1)(x, transmute_weights(3.8, -1)(x, w)))
  expect_equal(
    generalized_mean(1)(xna, w, na.rm = TRUE),
    generalized_mean(-0.04)(
      xna, transmute_weights(1, -0.04)(xna, w), na.rm = TRUE
    )
  )
  expect_equal(
    generalized_mean(9)(x, wna, na.rm = TRUE),
    generalized_mean(-4)(x, transmute_weights(9, -4)(x, wna), na.rm = TRUE)
  )
  expect_equal(
    generalized_mean(0)(xna, w, na.rm = TRUE),
    generalized_mean(0)(xna, transmute_weights(0, 0)(xna, w), na.rm = TRUE)
  )
})

test_that("generalized means work with factored weights", {
  expect_equal(
    generalized_mean(-0.9)(x * a),
    generalized_mean(-0.9)(x) *
      generalized_mean(-0.9)(a, factor_weights(-0.9)(x))
  )
  expect_equal(
    generalized_mean(6)(x * a, w),
    generalized_mean(6)(x, w) *
      generalized_mean(6)(a, factor_weights(6)(x, w))
  )
  expect_equal(
    generalized_mean(2.2)(xna * a, w, na.rm = TRUE),
    generalized_mean(2.2)(xna, w, na.rm = TRUE) *
      generalized_mean(2.2)(a, factor_weights(2.2)(xna, w), na.rm = TRUE)
  )
  expect_equal(
    generalized_mean(0)(xna * a, w, na.rm = TRUE),
    generalized_mean(0)(xna, w, na.rm = TRUE) *
      generalized_mean(0)(a, factor_weights(0)(xna, w), na.rm = TRUE)
  )
  expect_equal(
    generalized_mean(1)(x * a, w),
    generalized_mean(1)(x, update_weights(a, w)) * generalized_mean(1)(a, w)
  )
})

test_that("Lehmer mean satifies key properties", {
  # Pythagorean means
  expect_equal(lehmer_mean(1)(x, w),
               weighted.mean(x, w))
  expect_equal(lehmer_mean(1)(xna, na.rm = TRUE),
               weighted.mean(xna, na.rm = TRUE))
  expect_equal(lehmer_mean(0)(x, w),
               1 / weighted.mean(1 / x, w))
  expect_equal(lehmer_mean(0.5)(x[1:2]),
               sqrt(prod(x[1:2])))
  # Reversal
  expect_equal(lehmer_mean(5)(x, w),
               1 / lehmer_mean(-4)(1 / x, w))
  expect_equal(lehmer_mean(-3)(x),
               1 / lehmer_mean(4)(1 / x))
  expect_equal(contraharmonic_mean(xna, w, na.rm = TRUE),
               1 / lehmer_mean(-1)(1 / xna, w, na.rm = TRUE))
  expect_equal(contraharmonic_mean(xna, na.rm = TRUE),
               1 / lehmer_mean(-1)(1 / xna, na.rm = TRUE))
})

test_that("logmeans agree with known values", {
  expect_equal(logmean(1, 1), 1)
  expect_equal(logmean(1, 0), 0)
  expect_equal(logmean(2, 1), 1 / log(2))
  expect_equal(generalized_logmean(2)(1, 2), 1.5)
  expect_equal(generalized_logmean(-1)(1, 2), sqrt(2))
  expect_equal(extended_mean(0, 0)(4, 4), 4)
  expect_equal(extended_mean(0, 0)(1, 4), 2)
})

test_that("logmeans satisfy key properties", {
  # Symmetry
  expect_equal(logmean(a, b), logmean(b, a))
  expect_equal(extended_mean(0, 1)(a, b), logmean(a, b))
  expect_equal(extended_mean(-0.1, 2.5)(a, b), extended_mean(2.5, -0.1)(b, a))
  expect_equal(extended_mean(0, 2)(a, b), extended_mean(2, 0)(b, a))
  expect_equal(extended_mean(0, 0)(a, b), extended_mean(0, 0)(b, a))
  expect_equal(extended_mean(1, 1)(a, b), extended_mean(1, 1)(b, a))
  # Identities
  expect_equal(generalized_logmean(-1)(a, b),
               apply(matrix(c(a, b), ncol = 2), 1, geometric_mean))
  expect_equal(generalized_logmean(2)(a, b),
               apply(matrix(c(a, b), ncol = 2), 1, arithmetic_mean))
  expect_equal(generalized_logmean(-2)(a, b),
               apply(
                 matrix(c(a, b), ncol = 2), 1,
                 function(x) (harmonic_mean(x) * geometric_mean(x)^2)^(1 / 3)
                 )
               )
  expect_equal(generalized_logmean(0.5)(a, b),
               apply(
                 matrix(c(a, b), ncol = 2), 1,
                 function(x) (arithmetic_mean(x) + geometric_mean(x)) / 2
                 )
               )
  expect_equal(logmean(a, b),
               apply(matrix(c(a, b), ncol = 2), 1, geometric_mean)^2 *
                 logmean(1 / a, 1 / b))
  expect_equal(extended_mean(-2, -1)(a, b),
               apply(matrix(c(a, b), ncol = 2), 1, harmonic_mean))
  expect_equal(extended_mean(-2, 2)(a, b),
               apply(matrix(c(a, b), ncol = 2), 1, geometric_mean))
  expect_equal(extended_mean(3.5, -3.5)(a, b),
               apply(matrix(c(a, b), ncol = 2), 1, geometric_mean))
  expect_equal(extended_mean(2, 2)(a, b),
               (a^a^2 / b^b^2)^(1 / (a^2 - b^2)) / exp(1)^(1 / 2))
  expect_equal(extended_mean(1, 1)(a, b),
               (a^a / b^b)^(1 / (a - b)) / exp(1))
  expect_equal(extended_mean(-0.5, -0.5)(a, b),
               (a^a^-0.5 / b^b^-0.5)^(1 / (a^-0.5 - b^-0.5)) / exp(1)^(-2))
})

test_that("logmeans handles corner cases correctly", {
  # Tolerance
  expect_equal(logmean(2, sqrt(2)^2), 2)
  expect_equal(generalized_logmean(0.9)(2, sqrt(2)^2), 2)
  expect_equal(generalized_logmean(1.1)(2, sqrt(2)^2), 2)
  expect_equal(generalized_logmean(-1)(harmonic_mean(3 / pi), 3 / pi),
               3 / pi) # no more warning
  # Recycling
  expect_identical(logmean(2:3, rep(sqrt(2)^2, 4)),
                   c(2, logmean(3, sqrt(2)^2), 2, logmean(3, sqrt(2)^2)))
})

test_that("nested means work as expected", {
  expect_equal(fisher_mean(x, wna, w, na.rm = TRUE),
               geometric_mean(c(arithmetic_mean(x, wna, na.rm = TRUE),
                                harmonic_mean(x, w))))
  expect_equal(nested_mean(-3, c(2, 0.3), 3:4)(xna, a, b, na.rm = TRUE),
               generalized_mean(-3)(
                 c(generalized_mean(2)(xna, a, na.rm = TRUE),
                   generalized_mean(0.3)(xna, b, na.rm = TRUE)),
                 3:4
                 )
               )
})

test_that("nested mean reduces to a generalized mean", {
  expect_equal(nested_mean(2, c(1, 0), c(2, 0))(x, a, b),
               arithmetic_mean(x, a))
  expect_equal(nested_mean(-2, c(1, 3), c(NA, 0.1))(x, w, wna, na.rm = TRUE),
               generalized_mean(3)(x, wna, na.rm = TRUE))
  expect_equal(nested_mean(-2, c(1, 3), c(NA, NA))(x), NA_real_)
  expect_equal(nested_mean(2, c(1, 1))(x, a, a), arithmetic_mean(x, a))
  expect_equal(nested_mean(-1, c(-1, -1))(x), harmonic_mean(x))
})

test_that("nested mean works with transmuted weights", {
  expect_equal(fisher_mean(x, w),
               generalized_mean(1)(x, nested_transmute(0, c(1, -1), 1)(x, w)))
  expect_equal(
    fisher_mean(xna, a, na.rm = TRUE),
    generalized_mean(1)(
      xna, nested_transmute(0, c(1, -1), 1)(xna, a), na.rm = TRUE
    )
  )
  expect_equal(
    nested_mean(-5, c(1.1, -1.1), 1:2)(x, xna, w, na.rm = TRUE),
    generalized_mean(0.2)(
      x, nested_transmute(-5, c(1.1, -1.1), 0.2, 1:2)(x, xna, w), na.rm = TRUE
    )
  )

  expect_equal(
    fisher_mean(x, a),
    generalized_mean(1)(x, nested_transmute2(0, c(1, -1), 1)(x, a))
  )
  expect_equal(
    fisher_mean(xna, a, na.rm = TRUE),
    generalized_mean(1)(
      xna, nested_transmute2(0, c(1, -1), 1)(xna, a), na.rm = TRUE
    )
  )
  expect_equal(
    nested_mean(-5, c(1.1, -0.1), 1:2)(x, xna, b, na.rm = TRUE),
    generalized_mean(0.2)(
      x, nested_transmute2(-5, c(1.1, -0.1), 0.2, 1:2)(x, xna, b), na.rm = TRUE
    )
  )

  expect_equal(
    nested_mean(-5, c(1.1, -0.1), c(NA, 2))(x, b, xna, na.rm = TRUE),
    generalized_mean(0.2)(
      x, nested_transmute(-5, c(1.1, -0.1), 0.2, c(NA, 2))(x, b, xna),
      na.rm = TRUE
    )
  )
  expect_equal(
    nested_mean(-5, c(1.1, -0.1), c(NA, 2))(x, b, xna, na.rm = TRUE),
    generalized_mean(0.2)(
      x, nested_transmute2(-5, c(1.1, -0.1), 0.2, c(NA, 2))(x, b, xna),
      na.rm = TRUE
    )
  )
  expect_equal(
    nested_mean(-5, c(1.1, -0.1), c(NA, NA))(x, b, xna, na.rm = TRUE),
    generalized_mean(0.2)(
      x, nested_transmute(-5, c(1.1, -0.1), 0.2, c(NA, NA))(x, b, xna),
      na.rm = TRUE
    )
  )
  expect_equal(
    nested_mean(-5, c(1.1, -0.1), c(NA, NA))(x, b, xna, na.rm = TRUE),
    generalized_mean(0.2)(
      x, nested_transmute2(-5, c(1.1, -0.1), 0.2, c(NA, NA))(x, b, xna),
      na.rm = TRUE
    )
  )
})

test_that("factory attributes don't propegate", {
  expect_equal(generalized_mean(c(a = 1))(1:5), 3)
  expect_equal(extended_mean(c(a = 1), 2)(1, 2), 1.5)
})

test_that("grouping and balacing work", {
  # Grouped means
  f <- letters[c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1, 5, 4, 3, 2, 1)]
  expect_equal(grouped(arithmetic_mean)(x, group = f), ave(x, f))
  expect_equal(grouped(arithmetic_mean, na.rm = TRUE)(xna, group = f),
               ave(xna, f, FUN = function(x) mean(x, na.rm = TRUE)))
  # Balanced means
  expect_equal(arithmetic_mean(xna, w, na.rm = TRUE),
            balanced(arithmetic_mean)(xna, w, na.rm = TRUE))
  expect_equal(arithmetic_mean(xna, w),
            balanced(arithmetic_mean)(xna, w))
  expect_equal(arithmetic_mean(x, xna, na.rm = TRUE),
            balanced(weighted.mean)(x, xna, na.rm = TRUE))
  expect_equal(
    balanced(fisher_mean)(
      c(1, NA, 3, 4), c(NA, 1, 1, 2), c(1, 2, NA, 4), na.rm = TRUE
    ), 4)
})

test_that("error happen when expected", {
  expect_error(suppressWarnings(generalized_mean("a")))
  expect_error(generalized_mean(1:2))
  expect_error(generalized_mean(NaN))
  expect_error(geometric_mean(1:5, 1:3))
  expect_error(extended_mean(1, NA))
  expect_error(extended_mean(NA, 1))
  expect_error(suppressWarnings(extended_mean("a", mtcars)))
  expect_error(nested_mean(NA, 1:2))
  expect_error(nested_mean(1, 1:2, 1:3))
})
