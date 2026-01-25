set.seed(4321)

x <- c(1, 2, 1, 0.5, 1, 10, 1, 0.5, 0.2, 0.05)

test_that("outlier methods work", {
  expect_equal(outliers(x, method = "fixed-cutof"), x > 2.5 | x < 1 / 2.5)
  expect_equal(
    outliers(x, method = "quartile"),
    x > median(x) + (quantile(x, 0.75) - quantile(x, 0.5)) * 2.5 |
      x < median(x) - (quantile(x, 0.5) - quantile(x, 0.25)) * 2.5
  )
  expect_equal(
    outliers(x, method = "resistant-fences"),
    x > quantile(x, 0.75) + (quantile(x, 0.75) - quantile(x, 0.25)) * 2.5 |
      x < quantile(x, 0.25) - (quantile(x, 0.75) - quantile(x, 0.25)) * 2.5
  )
  expect_equal(
    outliers(x, method = "kimber"),
    x > quantile(x, 0.75) + (quantile(x, 0.75) - quantile(x, 0.5)) * 2.5 |
      x < quantile(x, 0.25) - (quantile(x, 0.5) - quantile(x, 0.25)) * 2.5
  )
  expect_true(
    sum(outliers(x, method = "resistant-fences")) <=
      sum(outliers(x, method = "quartile"))
  )
  expect_equal(
    outliers(x, method = "robust-z"),
    abs(x - median(x)) / mad(x) > 2.5
  )

  expect_equal(outliers(integer(0), method = "tukey"), logical(0))
  expect_equal(outliers(2, method = "tukey"), FALSE)
  expect_equal(
    outliers(seq(0.1, 2, by = 0.2), method = "tukey"),
    c(TRUE, rep(FALSE, 8), TRUE)
  )
  expect_equal(
    outliers(c(NA, 1, 2, 3), method = "tukey"),
    c(NA, TRUE, FALSE, TRUE)
  )
})

test_that("outliers work with NAs", {
  expect_identical(
    outliers(x, method = "resistant-fences"),
    outliers(c(NA, x), method = "resistant-fences")[-1]
  )
  expect_identical(
    outliers(x, method = "quartile"),
    outliers(c(NA, x), method = "quartile")[-1]
  )
  expect_identical(
    outliers(x, method = "robust-z"),
    outliers(c(NA, x), method = "robust-z")[-1]
  )
  expect_identical(
    outliers(x, method = "tukey"),
    outliers(c(NA, x), method = "tukey")[-1]
  )
  expect_identical(
    outliers(x, method = "kimber"),
    outliers(c(NA, x), method = "kimber")[-1]
  )
})

test_that("hb transform works", {
  expect_equal(
    hb_transform(x),
    ifelse(x < median(x), 1 - median(x) / x, x / median(x) - 1)
  )
  expect_equal(hb_transform(x), hb_transform(c(NA, x)[-1]))
})

test_that("recycling gives an error", {
  expect_error(outliers(x, cl = rep(2.5, 10)))
  expect_error(outliers(x, cu = rep(2.5, 0)))
  expect_error(outliers(x, a = rep(0, 11)))
})
