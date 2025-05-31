set.seed(1111)
p1 <- runif(15, 1, 5)
p0 <- rlnorm(15)
q1 <- rnorm(15)^2
q0 <- rnorm(15, mean = 2)^2
pb <- rlnorm(15)
qb <- runif(15, 2, 4)

test_that("arithmetic indexes work", {
  expect_equal(
    arithmetic_index("Carli")(p1, p0),
    mean(p1 / p0)
  )
  expect_equal(
    arithmetic_index("Dutot")(p1, p0),
    sum(p1) / sum(p0)
  )
  expect_equal(
    arithmetic_index("Laspeyres")(p1, p0, q0),
    sum(p1 * q0) / sum(p0 * q0)
  )
  expect_equal(
    arithmetic_mean(p1 / p0, index_weights("HybridPaasche")(p0, q1)),
    sum(p1 * q1) / sum(p0 * q1)
  )
  expect_equal(
    arithmetic_index("Palgrave")(p1, p0, q1),
    weighted.mean(p1 / p0, p1 * q1 / sum(p1 * q1))
  )
  expect_equal(
    arithmetic_index("Drobisch")(p1, p0, q1, q0),
    0.5 * sum(p1 * q0) / sum(p0 * q0) +
      0.5 * sum(p1 * q1) / sum(p0 * q1)
  )
  expect_equal(
    arithmetic_index("Unnamed")(p1, p0, q1, q0),
    weighted.mean(
      p1 / p0,
      0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1)
    )
  )
  expect_equal(
    arithmetic_index("Walsh1")(p1, p0, q1, q0),
    sum(p1 * sqrt(q0 * q1)) / sum(p0 * sqrt(q0 * q1))
  )
  expect_equal(
    arithmetic_index("MarshallEdgeworth")(p1, p0, q1, q0),
    sum(p1 * (q0 + q1) / 2) / sum(p0 * (q0 + q1) / 2)
  )
  expect_equal(
    arithmetic_index("GearyKhamis")(p1, p0, q1, q0),
    sum(p1 * 1 / (0.5 / q0 + 0.5 / q1)) /
      sum(p0 * 1 / (0.5 / q0 + 0.5 / q1))
  )
  expect_equal(
    arithmetic_index("Lowe")(p1, p0, qb),
    sum(p1 * qb) / sum(p0 * qb)
  )
  expect_equal(
    arithmetic_index("Young")(p1, p0, pb, qb),
    weighted.mean(p1 / p0, pb * qb / sum(pb * qb))
  )
  expect_equal(
    arithmetic_index("HybridCSWD")(p1, p0),
    sum(sqrt(p1 / p0)) / sum(sqrt(p0 / p1))
  )
})

test_that("geometric indexes work", {
  expect_equal(
    geometric_index("Jevons")(p1, p0),
    prod((p1 / p0)^(1 / length(p0)))
  )
  expect_equal(
    geometric_index("Laspeyres")(p1, p0, q0),
    geometric_mean(p1 / p0, p0 * q0 / sum(p0 * q0))
  )
  expect_equal(
    geometric_index("Paasche")(p1, p0, q1),
    geometric_mean(p1 / p0, p1 * q1 / sum(p1 * q1))
  )
  expect_equal(
    geometric_index("Young")(p1, p0, pb, qb),
    geometric_mean(p1 / p0, pb * qb / sum(pb * qb))
  )
  expect_equal(
    geometric_index("Tornqvist")(p1, p0, q1, q0),
    geometric_mean(p1 / p0, 0.5 * p0 * q0 / sum(p0 * q0) +
      0.5 * p1 * q1 / sum(p1 * q1))
  )
  expect_equal(
    geometric_index("Vartia1")(p1, p0, q1, q0),
    prod((p1 / p0)^(logmean(p0 * q0, p1 * q1) /
      logmean(sum(p0 * q0), sum(p1 * q1))))
  )
  geometric_index("Vartia1")(p1, p0, q1, q0) ==
    geometric_index("MontgomeryVartia")(p1, p0, q1, q0)
  expect_equal(
    geometric_index("Vartia2")(p1, p0, q1, q0),
    geometric_mean(
      p1 / p0,
      logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1))
    )
  )
  geometric_index("Vartia2")(p1, p0, q1, q0) ==
    geometric_index("SatoVartia")(p1, p0, q1, q0)
  expect_equal(
    geometric_index("Walsh2")(p1, p0, q1, q0),
    geometric_mean(p1 / p0, sqrt(p1 * q1 * p0 * q0))
  )
})

test_that("harmonic indexes work", {
  expect_equal(
    harmonic_index("Coggeshall")(p1, p0),
    harmonic_mean(p1 / p0)
  )
  expect_equal(
    harmonic_index("Laspeyres")(p1, p0, q0),
    harmonic_mean(p1 / p0, p0 * q0 / sum(p0 * q0))
  )
  expect_equal(
    harmonic_index("Paasche")(p1, p0, q1),
    harmonic_mean(p1 / p0, p1 * q1 / sum(p1 * q1))
  )
  expect_equal(
    arithmetic_index("Laspeyres")(p1, p0, q0),
    harmonic_mean(p1 / p0, index_weights("HybridLaspeyres")(p1, q0))
  )
  expect_equal(
    harmonic_index("Young")(p1, p0, pb, qb),
    harmonic_mean(p1 / p0, pb * qb / sum(pb * qb))
  )
})

test_that("the other indexes work", {
  expect_equal(
    fisher_index(p1, p0, q1, q0),
    geometric_mean(
      c(sum(p1 * q0) / sum(p0 * q0), sum(p1 * q1) / sum(p0 * q1))
    )
  )
  expect_equal(
    hlp_index(p1, p0, q1, q0),
    harmonic_mean(
      c(sum(p1 * q0) / sum(p0 * q0), sum(p1 * q1) / sum(p0 * q1))
    )
  )
  expect_equal(
    lm_index(1.5)(p1, p0, q0),
    generalized_mean(-0.5)(p1 / p0, p0 * q0 / sum(p0 * q0))
  )
  expect_equal(
    cswd_index(p1, p0),
    sqrt(arithmetic_mean(p1 / p0) * harmonic_mean(p1 / p0))
  )
  expect_equal(
    cswdb_index(p1, p0, q1, q0),
    sqrt(
      arithmetic_mean(p1 / p0) / arithmetic_mean(q1 / q0) *
        arithmetic_mean(p1 * q1 / (p0 * q0))
    )
  )
  expect_equal(
    bw_index(p1, p0),
    arithmetic_mean(sqrt(p1 / p0)) * harmonic_mean(sqrt(p1 / p0))
  )
  expect_equal(
    stuvel_index(4, 4)(p1, p0, q1, q0),
    stuvel_index(1, 1)(p1, p0, q1, q0)
  )
  expect_true(
    stuvel_index(4, 4)(p1, p0, q1, q0) != stuvel_index(2, 1)(p1, p0, q1, q0)
  )
  expect_equal(
    arithmetic_agmean_index(0.25)(p1, p0, q0),
    0.25 * geometric_index("Laspeyres")(p1, p0, q0) +
      0.75 * laspeyres_index(p1, p0, q0)
  )
  expect_equal(
    geometric_agmean_index(0.25)(p1, p0, q0),
    geometric_index("Laspeyres")(p1, p0, q0)^0.25 *
      laspeyres_index(p1, p0, q0)^0.75
  )
  expect_equal(
    round(lehr_index(c(4, 2), c(2, 1), c(1, 16), c(8, 8)), 4), 1.6154
  )
  expect_equal(
    martini_index(0.5)(p1, p0, q1, q0),
    arithmetic_index("Walsh1")(p1, p0, q1, q0)
  )
  expect_equal(
    martini_index(0)(p1, p0, q1, q0),
    laspeyres_index(p1, p0, q0)
  )
})

test_that("quantity indexes work", {
  expect_equal(
    fisher_index(p1, p0, q1, q0),
    quantity_index(fisher_index)(p1, p0, q1, q0)
  )
  expect_equal(
    lm_index(1.5)(p1, p0, q0),
    quantity_index(lm_index(1.5))(p1, p0, p0 = q0)
  )
  expect_equal(
    jevons_index(p1, p0),
    quantity_index(jevons_index)(q1 = p1, q0 = p0)
  )
  expect_equal(
    laspeyres_index(q1, q0, p0),
    quantity_index(laspeyres_index)(q1, q0 = q0, p0)
  )
  expect_equal(
    index_weights("Vartia1")(p1, p0, q1, q0),
    quantity_index(index_weights("Vartia1"))(q1 = p1, p0, p1 = q1, q0)
  )
  expect_equal(
    fisher_index(q1, p1 = p1, p0 = p0, q0) *
      quantity_index(fisher_index)(q1, p1 = p1, p0 = p0, q0),
    sum(p1 * q1) / sum(p0 * q0)
  )
})

test_that("vartia1 weights are less than 1", {
  expect_true(sum(index_weights("Vartia1")(p1, p0, q0, q1)) < 1)
})

test_that("arguments with different lengths give an error", {
  expect_error(jevons_index(1:4, 1:5))
  expect_error(index_weights("Vartia1")(1, 2, 3, 1:4))
})
