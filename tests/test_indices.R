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
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Carli") - mean(p1 / p0)) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Dutot") - sum(p1) / sum(p0)) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Laspeyres") - sum(p1 * q0) / sum(p0 * q0)) < .Machine$double.eps^0.5
    abs(mean_arithmetic(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, "HybridPaasche")) - sum(p1 * q1) / sum(p0 * q1)) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Palgrave") - weighted.mean(p1 / p0, p1 * q1 / sum(p1 * q1))) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Drobish") - 0.5 * sum(p1 * q0) / sum(p0 * q0) - 0.5 * sum(p1 * q1) / sum(p0 * q1)) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Unnamed") - weighted.mean(p1 / p0, 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 *p1 * q1 / sum(p1 * q1))) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Walsh1") - sum(p1 * sqrt(q0 * q1)) / sum(p0 * sqrt(q0 * q1))) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "MarshallEdgeworth") - sum(p1 * (q0 + q1) / 2) / sum(p0 * (q0 + q1) / 2)) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "GearyKhamis") - sum(p1 * 1 / (0.5 / q0 + 0.5 / q1)) / sum(p0 * 1 / (0.5 / q0 + 0.5 / q1))) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Lowe") - sum(p1 * qb) / sum(p0 * qb)) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Young") - weighted.mean(p1 / p0, pb * qb / sum(pb * qb))) < .Machine$double.eps^0.5
  },
  local = getNamespace("gpindex")
)

#---- Tests for geometric indices ----
stopifnot(
  exprs = {
    abs(index_geometric(p1, p0, q1, q0, pb, qb, "Jevons") - prod((p1 / p0)^(1/length(p0)))) < .Machine$double.eps^0.5
    abs(index_geometric(p1, p0, q1, q0, pb, qb, "Laspeyres") - mean_geometric(p1 / p0, p0 * q0 / sum(p0 * q0))) < .Machine$double.eps^0.5
    abs(index_geometric(p1, p0, q1, q0, pb, qb, "Paasche") - mean_geometric(p1 / p0, p1 * q1 / sum(p1 * q1))) < .Machine$double.eps^0.5
    abs(index_geometric(p1, p0, q1, q0, pb, qb, "Young") - mean_geometric(p1 / p0, pb * qb / sum(pb * qb))) < .Machine$double.eps^0.5
    abs(index_geometric(p1, p0, q1, q0, pb, qb, "Tornqvist") - mean_geometric(p1 / p0, 0.5 * p0 * q0 / sum(p0 * q0) + 0.5 *p1 * q1 / sum(p1 * q1))) < .Machine$double.eps^0.5
    abs(index_geometric(p1, p0, q1, q0, pb, qb, "Vartia1") - mean_geometric(p1 / p0, logmean(p0 * q0, p1 * q1) / logmean(sum(p0 * q1), sum(p1 * q1)), scale = FALSE)) < .Machine$double.eps^0.5
    index_geometric(p1, p0, q1, q0, pb, qb, "Vartia1") == index_geometric(p1, p0, q1, q0, pb, qb, "MontgomeryVartia")
    abs(index_geometric(p1, p0, q1, q0, pb, qb, "Vartia2") - mean_geometric(p1 / p0, logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)))) < .Machine$double.eps^0.5
    index_geometric(p1, p0, q1, q0, pb, qb, "Vartia2") == index_geometric(p1, p0, q1, q0, pb, qb, "SatoVartia")
    abs(index_geometric(p1, p0, q1, q0, pb, qb, "Walsh2") - mean_geometric(p1 / p0, sqrt(p1 * q1 * p0 * q0))) < .Machine$double.eps^0.5
    },
  local = getNamespace("gpindex")
)

#---- Tests for harmonic indices ----
stopifnot(
  exprs = {
    abs(index_harmonic(p1, p0, q1, q0, pb, qb, "Coggeshall") - mean_harmonic(p1 / p0)) < .Machine$double.eps^0.5
    abs(index_harmonic(p1, p0, q1, q0, pb, qb, "Laspeyres") - mean_harmonic(p1 / p0, p0 * q0 / sum(p0 * q0))) < .Machine$double.eps^0.5
    abs(index_harmonic(p1, p0, q1, q0, pb, qb, "Paasche") - mean_harmonic(p1 / p0, p1 * q1 / sum(p1 * q1))) < .Machine$double.eps^0.5
    abs(index_arithmetic(p1, p0, q1, q0, pb, qb, "Laspeyres") - mean_harmonic(p1 / p0, index_weights(p1, p0, q1, q0, pb, qb, "HybridLaspeyres"))) < .Machine$double.eps^0.5
  },
  local = getNamespace("gpindex")
)

#---- Tests for the other indices ----
stopifnot(
  exprs = {
    abs(index_fisher(p1, p0, q1, q0) - mean_geometric(c(sum(p1 * q0) / sum(p0 * q0), sum(p1 * q1) / sum(p0 * q1)))) < .Machine$double.eps^0.5
    abs(index_hlp(p1, p0, q1, q0) - mean_harmonic(c(sum(p1 * q0) / sum(p0 * q0), sum(p1 * q1) / sum(p0 * q1)))) < .Machine$double.eps^0.5
    
  },
  local = getNamespace("gpindex")
)

#---- Tests for contributions ----
stopifnot(
  exprs = {
    abs(index_fisher(p1, p0, q1, q0) - sum(contribution_fisher(p1, p0, q1, q0))) < .Machine$double.eps^0.5
  },
  local = getNamespace("gpindex")
)
    