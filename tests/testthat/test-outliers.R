set.seed(4321)

x <- c(1, 2, 1, 0.5, 1, 10, 1, 0.5, 0.2, 0.05)

test_that("outlier methods work", {
  expect_equal(fixed_cutoff(x), x > 2.5 | x < 1 / 2.5)
  expect_equal(
    quartile_method(x),
    x > median(x) + (quantile(x, 0.75) - quantile(x, 0.5)) * 2.5 |
      x < median(x) - (quantile(x, 0.5) - quantile(x, 0.25)) * 2.5
  )
  expect_equal(
    quartile_method(x, a = c(0, 1)),
    x > median(x) + c((quantile(x, 0.75) - quantile(x, 0.5)), median(x)) * 2.5 |
      x < median(x) - c((quantile(x, 0.5) - quantile(x, 0.25)), median(x)) * 2.5
  )
  expect_equal(
    resistant_fences(x),
    x > quantile(x, 0.75) + (quantile(x, 0.75) - quantile(x, 0.25)) * 2.5 |
      x < quantile(x, 0.25) - (quantile(x, 0.75) - quantile(x, 0.25)) * 2.5
  )
  expect_true(sum(resistant_fences(x)) <= sum(quartile_method(x)))
  expect_equal(robust_z(x), abs(x - median(x)) / mad(x) > 2.5)

  expect_equal(tukey_algorithm(integer(0)), logical(0))
  expect_equal(tukey_algorithm(2), FALSE)
  expect_equal(
    tukey_algorithm(seq(0.1, 2, by = 0.2)), c(TRUE, rep(FALSE, 8), TRUE)
  )
  expect_equal(tukey_algorithm(c(NA, 1, 2, 3)), c(NA, TRUE, FALSE, TRUE))
})

test_that("hb transform works", {
  expect_equal(hb_transform(x),
               ifelse(x < median(x), 1 - median(x) / x, x / median(x) - 1))
})
