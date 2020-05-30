# Some data for tests
set.seed(1111)
p1 <- runif(15, 1, 5)
p0 <- rlnorm(15)
q1 <- rnorm(15)^2
q0 <- rnorm(15, mean = 2)^2
pb <- rlnorm(15)
qb <- runif(15, 2, 4)

#---- Tests for arithmetic indices ----
stopifnot(
  exprs = {
    all.equal(index_arithmetic(p1, p0, q1, q0, "Carli"),
              mean(p1 / p0))
    all.equal(index_arithmetic(p1, p0, q1, q0, "Dutot"),
              sum(p1) / sum(p0))
    all.equal(index_arithmetic(p1, p0, q1, q0, "Laspeyres"),
              sum(p1 * q0) / sum(p0 * q0))
    all.equal(mean_arithmetic(p1 / p0, index_weights(p1, p0, q1, q0, "HybridPaasche")),
              sum(p1 * q1) / sum(p0 * q1))
    all.equal(index_arithmetic(p1, p0, q1, q0, "Palgrave"),
              weighted.mean(p1 / p0, p1 * q1 / sum(p1 * q1)))
    all.equal(index_arithmetic(p1, p0, q1, q0, "Drobish"),
              0.5 * sum(p1 * q0) / sum(p0 * q0) + 0.5 * sum(p1 * q1) / sum(p0 * q1))
    all.equal(index_arithmetic(p1, p0, q1, q0, "Unnamed"),
              weighted.mean(p1 / p0, 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1)))
    all.equal(index_arithmetic(p1, p0, q1, q0, "Walsh1"),
              sum(p1 * sqrt(q0 * q1)) / sum(p0 * sqrt(q0 * q1)))
    all.equal(index_arithmetic(p1, p0, q1, q0, "MarshallEdgeworth"),
              sum(p1 * (q0 + q1) / 2) / sum(p0 * (q0 + q1) / 2))
    all.equal(index_arithmetic(p1, p0, q1, q0, "GearyKhamis"),
              sum(p1 * 1 / (0.5 / q0 + 0.5 / q1)) / sum(p0 * 1 / (0.5 / q0 + 0.5 / q1)))
    all.equal(index_lowe(p1, p0, qb),
              sum(p1 * qb) / sum(p0 * qb))
    all.equal(index_young(p1, p0, pb, qb),
              weighted.mean(p1 / p0, pb * qb / sum(pb * qb)))
  },
  local = getNamespace("gpindex")
)

#---- Tests for geometric indices ----
stopifnot(
  exprs = {
    all.equal(index_geometric(p1, p0, q1, q0, "Jevons"),
              prod((p1 / p0)^(1 / length(p0))))
    all.equal(index_geometric(p1, p0, q1, q0,"Laspeyres"),
              mean_geometric(p1 / p0, p0 * q0 / sum(p0 * q0)))
    all.equal(index_geometric(p1, p0, q1, q0,  "Paasche"),
              mean_geometric(p1 / p0, p1 * q1 / sum(p1 * q1)))
    all.equal(index_geometric(p1, p0, q1, q0, "Tornqvist"),
              mean_geometric(p1 / p0, 0.5 * p0 * q0 / sum(p0 * q0) +
                               0.5 * p1 * q1 / sum(p1 * q1)))
    all.equal(index_geometric(p1, p0, q1, q0, "Vartia1"),
              mean_geometric(p1 / p0, logmean(p0 * q0, p1 * q1) /
                               logmean(sum(p0 * q1), sum(p1 * q1)), scale = FALSE))
    index_geometric(p1, p0, q1, q0,  "Vartia1") ==
      index_geometric(p1, p0, q1, q0, "MontgomeryVartia")
    all.equal(index_geometric(p1, p0, q1, q0,  "Vartia2"),
              mean_geometric(p1 / p0, logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1))))
    index_geometric(p1, p0, q1, q0, "Vartia2") ==
      index_geometric(p1, p0, q1, q0, "SatoVartia")
    all.equal(index_geometric(p1, p0, q1, q0, "Walsh2"),
              mean_geometric(p1 / p0, sqrt(p1 * q1 * p0 * q0)))
    },
  local = getNamespace("gpindex")
)

#---- Tests for harmonic indices ----
stopifnot(
  exprs = {
    all.equal(index_harmonic(p1, p0, q1, q0, "Coggeshall"),
              mean_harmonic(p1 / p0))
    all.equal(index_harmonic(p1, p0, q1, q0, "Laspeyres"),
              mean_harmonic(p1 / p0, p0 * q0 / sum(p0 * q0)))
    all.equal(index_harmonic(p1, p0, q1, q0, "Paasche"),
              mean_harmonic(p1 / p0, p1 * q1 / sum(p1 * q1)))
    all.equal(index_arithmetic(p1, p0, q1, q0, "Laspeyres"),
              mean_harmonic(p1 / p0, index_weights(p1, p0, q1, q0, "HybridLaspeyres")))
  },
  local = getNamespace("gpindex")
)

#---- Tests for the other indices ----
stopifnot(
  exprs = {
    all.equal(index_fisher(p1, p0, q1, q0),
              mean_geometric(c(sum(p1 * q0) / sum(p0 * q0), sum(p1 * q1) / sum(p0 * q1))))
    all.equal(index_hlp(p1, p0, q1, q0),
              mean_harmonic(c(sum(p1 * q0) / sum(p0 * q0), sum(p1 * q1) / sum(p0 * q1))))
    all.equal(index_lm(p1, p0, q0, 1.5), mean_generalized(p1 / p0, p0 * q0 / sum(p0 * q0), -0.5))
    all.equal(index_cswd(p1, p0), sqrt(mean_arithmetic(p1 / p0) * mean_harmonic(p1 / p0)))
    all.equal(index_cswdb(p1, p0, q1, q0), sqrt(mean_arithmetic(p1 / p0) / mean_arithmetic(q1 / q0) * mean_arithmetic(p1 * q1 / (p0 * q0))))
    all.equal(index_bw(p1, p0), mean_arithmetic(sqrt(p1 / p0)) * mean_harmonic(sqrt(p1 / p0)))
  },
  local = getNamespace("gpindex")
)

#---- Tests for contributions ----
stopifnot(
  exprs = {
    all.equal(index_fisher(p1, p0, q1, q0),
              sum(contribution_fisher(p1, p0, q1, q0)))
  },
  local = getNamespace("gpindex")
)