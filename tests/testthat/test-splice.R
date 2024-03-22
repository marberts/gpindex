x <- list(1:3, 3:5, 5:7)

test_that("key splice methods works", {
  # Movement splice.
  expect_equal(splice_index(x, 3), c(1, 2, 6, 30, 210))
  # Window splice.
  expect_equal(splice_index(x, 1), c(1, 2, 6, 60, 420))
  # Mean splice.
  expect_equal(
    splice_index(x),
    c(1, 2, 6, geometric_mean(c(60, 40, 30)),
      geometric_mean(c(420, 252, 7 * geometric_mean(c(60, 40, 30)))))
  )
})

test_that("result length is correct", {
  expect_equal(
    splice_index(x[-1], 3, c(1, 1, 1, 1, 2, 3)),
    c(1, 1, 1, 1, 2, 6, 30, 210)
  )
  expect_equal(
    splice_index(x[-1], 1, c(1, 1, 1, 1, 2, 3)),
    c(1, 1, 1, 1, 2, 6, 60, 420)
  )
})

test_that("NAs return NA", {
  expect_equal(splice_index(x, c(1, NA, 4)), c(1, 2, 6, NA, NA))
  x[[2]][2] <- NA
  expect_equal(splice_index(x), c(1, 2, 6, NA, NA))
  expect_equal(splice_index(x, 3), c(1, 2, 6, 30, 210))
})

test_that("corner cases work", {
  expect_equal(splice_index(list()), numeric(0))
  expect_equal(splice_index(list(), initial = 1:5), cumprod(1:5))
  expect_equal(splice_index(list(1:5)), cumprod(1:5))
  expect_equal(splice_index(list(1, 2, 3, 4, 5)), cumprod(1:5))
})

test_that("errors work", {
  expect_error(splice_index(list(1:3, 1:2)))
  expect_error(splice_index(list(1:3, 1:3), initial = 1:2))
})