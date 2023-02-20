id <- letters[c(5:1, 1:5)]
period <- rep(1:2, each = 5)

test_that("offsetting periods works", {
  # Simple cases
  expect_identical(back_period(NULL, NULL), integer(0))
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
  expect_identical(back_period(replace(period, 2, NA), id),
                   c(1L, NA, 3L, 4L, 5L, 5L, 4L, 3L, NA, 1L))
  expect_identical(back_period(period[-1], id[-1]),
                   c(1L, 2L, 3L, 4L, 4L, 3L, 2L, 1L, NA))
  expect_identical(back_period(factor(period, levels = NA), id),
                   rep(NA_integer_, 10))
  # NA products shouldn't trigger a warning
  expect_identical(back_period(period, replace(id, 1:2, NA)),
                   c(NA, NA, 3L, 4L, 5L, 5L, 4L, 3L, NA, NA))
})

test_that("ambiguou products trigger a wanring", {
  expect_warning(base_period(c(1, 1, 2, 3)))
})