set.seed(4321)
x <- rnorm(15)^2
xna <- replace(x, 2, NA)
w <- runif(15, 0, 2)
f <- factor(sample(letters[1:3], 15, TRUE))

test_that("weights transmute correctly", {
  expect_equal(transmute_weights(2, 2)(x), rep(1 / 15, length(x)))
  expect_equal(transmute_weights(2, 2)(c(1:3, NA)), c(1, 1, 1, NA) / 3)
  expect_equal(
    transmute_weights(0, 0)(xna, w),
    scale_weights(replace(w, 2, NA))
  )
  expect_equal(transmute_weights(2, 1)(c(1, NA)), c(1, NA))
  expect_equal(transmute_weights(-1, 1)(x, w), scale_weights(w / x))
  expect_equal(transmute_weights(1, -1)(xna, w), scale_weights(w * xna))
  expect_equal(
    transmute_weights(7, -3)(x, transmute_weights(-3, 7)(x, w)),
    scale_weights(w)
  )
  expect_equal(
    grouped(transmute_weights(1, 2))(x, w, group = f),
    unsplit(Map(transmute_weights(1, 2), split(x, f), split(w, f)), f)
  )
  
  expect_error(transmute_weights(1, 1)(1:5, 1:4))
  expect_error(transmute_weights(1, 2)(1:5, 1:4))
})

test_that("contributions work correctly", {
  expect_equal(arithmetic_contributions(1:4), c(0, 0.25, 0.5, 0.75))
  expect_equal(harmonic_contributions(1:4), c(0, 0.24, 0.32, 0.36))
  expect_equal(geometric_contributions(c(1, 4)), c(0, 1))
  expect_equal(sum(contributions(-3.75)(x, w)),
               generalized_mean(-3.75)(x, w) - 1)
  expect_equal(sum(contributions(3.75)(xna, w), na.rm = TRUE),
               generalized_mean(3.75)(xna, w, na.rm = TRUE) - 1)
  expect_equal(
    as.numeric(tapply(grouped(geometric_contributions)(x, group = f), f, sum)),
    as.numeric(tapply(x, f, geometric_mean) - 1)
  )
})

test_that("weights factor correctly", {
  expect_equal(factor_weights(0)(c(1, NA)), c(1, NA))
  expect_equal(factor_weights(0)(x), rep(1, length(x)))
  expect_equal(factor_weights(0)(x, w), w)
  expect_equal(update_weights(xna, w), xna * w)
  expect_equal(grouped(update_weights)(x, w, group = f), x * w)
  
  expect_error(factor_weights(2)(1:5, 1:4))
  expect_error(factor_weights(0)(1:5, 1:4))
})

test_that("weights scale correctly", {
  expect_equal(sum(scale_weights(w)), 1)
  expect_equal(scale_weights(c(1:2, NA)), c(1:2, NA) / 3)
})

test_that("nested contributions work correctly", {
  expect_equal(
    sum(nested_contributions(3, c(-1, 2), c(0.75, 0.25))(x)),
    generalized_mean(3)(
      c(harmonic_mean(x), generalized_mean(2)(x)),
      c(0.75, 0.25)
    ) - 1
  )
  expect_equal(
    sum(nested_contributions2(3, c(-1, 2), c(0.75, 0.25))(x)),
    generalized_mean(3)(
      c(harmonic_mean(x), generalized_mean(2)(x)),
      c(0.75, 0.25)
    ) - 1
  )

  expect_equal(
    sum(nested_contributions(0, c(1, -1), c(0.5, 0.5))(x)),
    prod(sqrt(c(harmonic_mean(x), arithmetic_mean(x)))) - 1
  )
  expect_equal(
    nested_contributions(1, c(0, -1), c(1, 2))(xna, x, w),
    nested_contributions2(1, c(0, -1), c(1, 2))(xna, x, w)
  )

  expect_equal(
    sum(nested_contributions(1, c(0, -1), c(1, 2))(xna, x, w), na.rm = TRUE),
    nested_mean(1, c(0, -1), c(1, 2))(xna, x, w, na.rm = TRUE) - 1
  )

  expect_equal(
    sum(nested_contributions(0, c(3, -2))(x, w, xna), na.rm = TRUE),
    nested_mean(0, c(3, -2))(x, w, xna, na.rm = TRUE) - 1
  )

  expect_equal(
    sum(nested_contributions2(0, c(3, -2))(x, w, xna), na.rm = TRUE),
    nested_mean(0, c(3, -2))(x, w, xna, na.rm = TRUE) - 1
  )

  expect_equal(fisher_contributions(1:2, c(NA, NA)),
               fisher_contributions2(1:2, c(NA, NA)))

  expect_equal(fisher_contributions(1:2, c(NA, NA)),
               harmonic_contributions(1:2))

  expect_equal(nested_contributions(3, c(-1, 2), c(0.75, NA))(x),
               nested_contributions2(3, c(-1, 2), c(0.75, NA))(x))

  expect_equal(nested_contributions(3, c(-1, 2), c(0.75, NA))(x),
               harmonic_contributions(x))

  expect_equal(nested_contributions(3, c(2, -1), c(NA, 0.75))(x, w2 = xna),
               nested_contributions2(3, c(2, -1), c(NA, 0.75))(x, w2 = xna))

  expect_equal(nested_contributions(3, c(2, -1), c(NA, 0.75))(x, w2 = xna),
               harmonic_contributions(x, xna))

  expect_equal(nested_contributions(3, c(-1, 2), c(NA, NA))(x),
               rep(NA_real_, length(x)))

  expect_equal(nested_contributions2(3, c(-1, 2), c(NA, NA))(x),
               rep(NA_real_, length(x)))
})
