id <- letters[c(5:1, 1:5)]
period <- rep(1:2, each = 5)

test_that("offsetting periods works", {
  # Length 0 inputs
  expect_identical(back_period(NULL, NULL), integer(0))
  expect_identical(back_period(numeric(0), factor(numeric(0), 1:3)), integer(0))
  expect_identical(back_period(factor(numeric(0), 1:3), numeric(0)), integer(0))
  expect_identical(
    back_period(factor(numeric(0), 1:3), factor(numeric(0), 1:3)),
    integer(0)
  )

  # Simple cases
  expect_identical(back_period(1:4), c(1L, 1L, 2L, 3L))
  expect_identical(base_period(1:4), rep(1L, 4))

  # Attributes shouldn't do anything
  expect_identical(back_period(matrix(1:4)), c(1L, 1L, 2L, 3L))

  # Change time periods
  expect_identical(back_period(factor(rep(1, 4), levels = 0:1), 1:4),
                   rep(NA_integer_, 4))
  expect_identical(back_period(factor(1:4, levels = 4:1)),
                   c(2L, 3L, 4L, 4L))

  # A more interesting case
  expect_identical(back_period(period, id),
                   c(1L, 2L, 3L, 4L, 5L, 5L, 4L, 3L, 2L, 1L))
  expect_identical(base_period(period, id),
                   c(1L, 2L, 3L, 4L, 5L, 5L, 4L, 3L, 2L, 1L))
  expect_identical(back_period(replace(period, 2, NA), id),
                   c(1L, NA, 3L, 4L, 5L, 5L, 4L, 3L, NA, 1L))
  expect_identical(back_period(period[-1], id[-1]),
                   c(1L, 2L, 3L, 4L, 4L, 3L, 2L, 1L, NA))
  expect_identical(back_period(factor(period, levels = NA), id),
                   rep(NA_integer_, 10))

  # Change time periods again
  expect_identical(back_period(factor(period, c(1, 3, 2)), id),
                   c(1L, 2L, 3L, 4L, 5L, NA, NA, NA, NA, NA))
  expect_identical(base_period(factor(period, c(1, 3, 2)), id),
                   c(1L, 2L, 3L, 4L, 5L, 5L, 4L, 3L, 2L, 1L))

  # NA products shouldn't trigger a warning
  expect_identical(back_period(period, replace(id, 1:2, NA)),
                   c(NA, NA, 3L, 4L, 5L, 5L, 4L, 3L, NA, NA))
})

test_that("not matching the first period works", {
  expect_identical(back_period(period, id, FALSE),
                   c(rep(NA, 5), 5:1))
  expect_identical(base_period(period, id, FALSE),
                   c(rep(NA, 5), 5:1))
})

test_that("ambiguous products trigger a warning", {
  expect_warning(base_period(c(1, 1, 2, 3)))
})

test_that("unequal lengths gives an error", {
  expect_error(base_period(1:5, 1:4))
})
