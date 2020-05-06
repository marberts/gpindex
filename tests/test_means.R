# stopifnot(
#   exprs = {
#     generalized_mean(Inf)(1:5) == 5
#     generalized_mean(-Inf)(1:5) == 1
#     generalized_mean(1)(1:5) == 3
#     generalized_mean(0L)(c(2, 2)) == 2
#     abs(generalized_mean(0)(1:5) - prod(1:5)^0.2) < .Machine$double.eps^0.5
#     abs(generalized_mean(-1)(1:100) - mean((1:100)^(-1))^(-1)) < .Machine$double.eps^0.5
#     abs(generalized_mean(-2)(1:100) - mean((1:100)^(-2))^(-1/2)) < .Machine$double.eps^0.5
#     abs(generalized_mean(2)(1:100) - mean((1:100)^(2))^(1/2)) < .Machine$double.eps^0.5
#     abs(geometric_mean(1:2) - sqrt(2)) < .Machine$double.eps^0.5
#     abs(geometric_mean(1:3, 1:3) - prod((1:3)^(1:3 / 6))) < .Machine$double.eps^0.5
#     geometric_mean(c(1, NA), na.rm = TRUE) == 1
#     is.na(geometric_mean(c(1, NA)))
#     abs(harmonic_mean(1:2) - 4 / 3) < .Machine$double.eps^0.5
#     abs(harmonic_mean(1:3, 1:3) - 2) < .Machine$double.eps^0.5
#     harmonic_mean(c(1, NA), na.rm = TRUE) == 1
#     is.na(harmonic_mean(c(1, NA)))
#     geometric_mean(1:100, 100:1) <= weighted.mean(1:100, 100:1)
#     geometric_mean(1:100, 100:1) >= harmonic_mean(1:100, 100:1)
#     quadratic_mean(1:100, 100:1) >= weighted.mean(1:100, 100:1)
#     cubic_mean(1:100, 100:1) >= quadratic_mean(1:100, 100:1)     
#     abs(quadratic_mean(1:100) - mean((1:100)^(2))^(1/2)) < .Machine$double.eps^0.5
#     abs(cubic_mean(1:100) - mean((1:100)^(3))^(1/3)) < .Machine$double.eps^0.5
#     is.nan(geometric_mean(integer(0)))
#     is.nan(geometric_mean(1:2, c(0, 0)))
#   }, 
#   local = getNamespace('rmeans')
# )