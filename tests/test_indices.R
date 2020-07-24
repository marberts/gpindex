# Some data for tests
set.seed(1111)
p1 <- runif(15, 1, 5)
p0 <- rlnorm(15)
q1 <- rnorm(15)^2
q0 <- rnorm(15, mean = 2)^2
pb <- rlnorm(15)
qb <- runif(15, 2, 4)

#---- Tests for arithmetic indexes ----
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

#---- Tests for geometric indexes ----
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
                               logmean(sum(p0 * q0), sum(p1 * q1)), scale = FALSE))
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

#---- Tests for harmonic indexes ----
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

#---- Tests for the other indexes ----
stopifnot(
  exprs = {
    all.equal(index_fisher(p1, p0, q1, q0),
              mean_geometric(c(sum(p1 * q0) / sum(p0 * q0), sum(p1 * q1) / sum(p0 * q1))))
    all.equal(index_hlp(p1, p0, q1, q0),
              mean_harmonic(c(sum(p1 * q0) / sum(p0 * q0), sum(p1 * q1) / sum(p0 * q1))))
    all.equal(index_lm(p1, p0, q0, 1.5), 
              mean_generalized(p1 / p0, p0 * q0 / sum(p0 * q0), -0.5))
    all.equal(index_cswd(p1, p0), 
              sqrt(mean_arithmetic(p1 / p0) * mean_harmonic(p1 / p0)))
    all.equal(index_cswdb(p1, p0, q1, q0), 
              sqrt(mean_arithmetic(p1 / p0) / mean_arithmetic(q1 / q0) * mean_arithmetic(p1 * q1 / (p0 * q0))))
    all.equal(index_bw(p1, p0), 
              mean_arithmetic(sqrt(p1 / p0)) * mean_harmonic(sqrt(p1 / p0)))
    all.equal(index_stuval(p1, p0, q1, q0, 4, 4), index_stuval(p1, p0, q1, q0, 1, 1))
    index_stuval(p1, p0, q1, q0, 4, 4) != index_stuval(p1, p0, q1, q0, 2, 1)
  },
  local = getNamespace("gpindex")
)

#---- Tests for contributions ----
# stopifnot(
#   exprs = {
#     all.equal(index_fisher(p1, p0, q1, q0),
#               sum(contribution_fisher(p1, p0, q1, q0)))
#     all(
#       vapply(
#         setdiff(types$geometric_index_types, c("Vartia1", "MontgomeryVartia")),
#         function(i)
#           all.equal(
#             index_geometric(p1, p0, q1, q0, i),
#               sum(contribution_geometric(p1, p0, q1, q0, type = i))
#             ),
#         logical(1)
#       )
#     )
#     all(
#       vapply(
#         types$harmonic_index_types,
#         function(i)
#           all.equal(
#             index_harmonic(p1, p0, q1, q0, i),
#             sum(contribution_harmonic(p1, p0, q1, q0, type = i))
#           ),
#         logical(1)
#       )
#     )
#   },
#   local = getNamespace("gpindex")
# )

# Test against values from tables 3.4, 3.6, and 3.12 in Balk (2008)
# Column P in table 3.4 is 1.3823 because of a rounding error

# Table 3.1 in Balk (2008), adapted from table 19.1 in PPI manual
price6 <- data.frame(t1 = c(1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
                     t2 = c(1.2, 3.0, 1.3, 0.7, 1.4, 0.8),
                     t3 = c(1.0, 1.0, 1.5, 0.5, 1.7, 0.6),
                     t4 = c(0.8, 0.5, 1.6, 0.3, 1.9, 0.4),
                     t5 = c(1.0, 1.0, 1.6, 0.1, 2.0, 0.2)
)

# Table 3.2 in Balk (2008), adapted from table 19.2 in PPI manual
quantity6 <- data.frame(t1 = c(1.0, 1.0, 2.0, 1.0, 4.5, 0.5),
                        t2 = c(0.8, 0.9, 1.9, 1.3, 4.7, 0.6),
                        t3 = c(1.0, 1.1, 1.8, 3.0, 5.0, 0.8),
                        t4 = c(1.2, 1.2, 1.9, 6.0, 5.6, 1.3),
                        t5 = c(0.9, 1.2, 2.0, 12.0, 6.5, 2.5)
)

stopifnot(
  exprs = {
    all(
      abs(
        mapply(
          function(p1, p0, q1, q0) {
            round(
              c(
                index_harmonic(p1, p0, q1, q0, "Laspeyres"),
                index_geometric(p1, p0, q1, q0, "Laspeyres"),
                index_arithmetic(p1, p0, q1, q0, "Laspeyres"),
                index_harmonic(p1, p0, q1, q0, "Paasche"),
                index_geometric(p1, p0, q1, q0, "Paasche"),
                index_arithmetic(p1, p0, q1, q0, "Palgrave"),
                index_fisher(p1, p0, q1, q0),
                index_geometric(p1, p0, q1, q0, "Tornqvist"),
                index_arithmetic(p1, p0, q1, q0, "MarshallEdgeworth"),
                index_arithmetic(p1, p0, q1, q0, "Walsh1"),
                index_geometric(p1, p0, q1, q0, "Vartia2"),
                index_geometric(p1, p0, q1, q0, "Vartia1"),
                index_stuval(p1, p0, q1, q0, 2, 2)
              ), 4)
          },
          price6, price6[1], quantity6, quantity6[1]
        ) -
          matrix(c(
            1.0000, 1.2542, 1.1346, 0.8732, 0.5556,
            1.0000, 1.3300, 1.2523, 1.1331, 1.0999,
            1.0000, 1.4200, 1.3450, 1.3550, 1.4400,
            1.0000, 1.3824, 1.2031, 1.0209, 0.7968,
            1.0000, 1.4846, 1.3268, 1.3282, 1.4153,
            1.0000, 1.6096, 1.4161, 1.5317, 1.6720,
            1.0000, 1.4011, 1.2721, 1.1762, 1.0712,
            1.0000, 1.4052, 1.2890, 1.2268, 1.2477,
            1.0000, 1.4010, 1.2656, 1.1438, 0.9801,
            1.0000, 1.4017, 1.2850, 1.2193, 1.1850,
            1.0000, 1.4018, 1.2897, 1.2335, 1.2540,
            1.0000, 1.4024, 1.2907, 1.2392, 1.2678,
            1.0000, 1.4042, 1.2742, 1.1551, 0.9770
          ), ncol = 5, byrow = TRUE) 
      ) <= 0.0001
    )
  },
  local = getNamespace("gpindex")
)

#---- Tests for weights ----
stopifnot(
  exprs = {
    sum(index_weights(p1, p0, q0, q1, type = "Vartia1")) < 1
    all.equal(sum(index_weights(p1, p0, q0, q1, type = "Laspeyres")), 1)
    length(index_weights(integer(0), type = "Carli")) == 0
  },
  local = getNamespace("gpindex")
)