# These tests are slow and not usually needed
# They really work the core computational functions
# Should return all TRUE

# set.seed(4132)
# x <- rlnorm(15)
# xna <- replace(rlnorm(15), c(4, 12), NA)
# w <- runif(15)
# a <- runif(15, 0, 5)
# b <- rlnorm(15)
# 
# # Simple implementations
# gmean <- function(x, w, r) {
#   if (r != 0) {
#     (weighted.mean(x^r, w, na.rm = TRUE))^(1 / r)
#   } else {
#     exp(weighted.mean(log(x), w, na.rm = TRUE))
#   }
# }
# 
# lmean <- function(x, w, r) {
#   w <- if (missing(w)) x^(r - 1) else w * x^(r - 1)
#   weighted.mean(x, w, na.rm = TRUE)
# }
# 
# emean <- function(a, b, r, s) {
#   if (r == 0 && s == 0) {
#     sqrt(a * b)
#   } else if (r == 0) {
#     ((a^s - b^s) / log(a / b) / s)^(1 / s)
#   } else if (s == 0) {
#     ((a^r - b^r) / log(a / b) / r)^(1 / r)
#   } else if (r == s) {
#     exp((a^r * log(a) - b^r * log(b)) / (a^r - b^r) - 1 / r)
#   } else {
#     ((a^s - b^s) / (a^r - b^r) * r / s)^(1 / (s - r))
#   }
# }
# 
# grid1 <- seq(-10, 10, by = 0.25)
# grid2 <- expand.grid(a = seq(-10, 10, by = 0.25), b = seq(-10, 10, by = 0.25))
# 
# # Generalized mean
# # Unweighted case
# all(vapply(grid1, function(r) {
#   all.equal(generalized_mean(r)(x), gmean(x, r = r))
# }, logical(1)))
# 
# all(vapply(grid1, function(r) {
#   all.equal(generalized_mean(r)(xna, na.rm = TRUE), gmean(xna, r = r))
# }, logical(1)))
# 
# # Weighted case
# all(vapply(grid1, function(r) {
#   all.equal(generalized_mean(r)(x, w), gmean(x, w, r))
# }, logical(1)))
# 
# all(vapply(grid1, function(r) {
#   all.equal(generalized_mean(r)(xna, w, na.rm = TRUE), gmean(xna, w, r))
# }, logical(1)))
# 
# # Lehmer mean
# # Unweighted case
# all(vapply(grid1, function(r) {
#   all.equal(lehmer_mean(r)(x), lmean(x, r = r))
# }, logical(1)))
# 
# all(vapply(grid1, function(r) {
#   all.equal(lehmer_mean(r)(xna, na.rm = TRUE), lmean(xna, r = r))
# }, logical(1)))
# 
# # Weighted case
# all(vapply(grid1, function(r) {
#   all.equal(lehmer_mean(r)(x, w), lmean(x, w, r))
# }, logical(1)))
# 
# all(vapply(grid1, function(r) {
#   all.equal(lehmer_mean(r)(xna, w, na.rm = TRUE), lmean(xna, w, r))
# }, logical(1)))
# 
# # Extended mean
# all(apply(grid2, 1, function(p) {
#   all.equal(extended_mean(p[1], p[2])(a, b),
#             emean(a, b, p[1], p[2]),
#             check.names = FALSE)
# }))
# 
# all(apply(grid2, 1, function(p) {
#   all.equal(extended_mean(p[1], p[2])(a, b),
#             emean(a, b, p[2], p[1]),
#             check.names = FALSE)
# }))
# 
# # Transmute weights
# # Unweighted case
# all(apply(grid2, 1, function(p) {
#   all.equal(generalized_mean(p[1])(x),
#             generalized_mean(p[2])(x, transmute_weights(p[1], p[2])(x)),
#             check.names = FALSE)
# }))
# all(apply(grid2, 1, function(p) {
#   all.equal(generalized_mean(p[1])(xna, na.rm = TRUE),
#             generalized_mean(p[2])(xna, transmute_weights(p[1], p[2])(xna), na.rm = TRUE),
#             check.names = FALSE)
# }))
# # Weighted case
# all(apply(grid2, 1, function(p) {
#   all.equal(generalized_mean(p[1])(x, w),
#             generalized_mean(p[2])(x, transmute_weights(p[1], p[2])(x, w)),
#             check.names = FALSE)
# }))
# all(apply(grid2, 1, function(p) {
#   all.equal(generalized_mean(p[1])(xna, w, na.rm = TRUE),
#             generalized_mean(p[2])(xna, transmute_weights(p[1], p[2])(xna, w), na.rm = TRUE),
#             check.names = FALSE)
# }))
# 
# # Factor weights
# # Unweighted case
# all(vapply(grid1,
#            function(r) {
#              all.equal(generalized_mean(r)(x * a),
#                        generalized_mean(r)(x) * generalized_mean(r)(a, factor_weights(r)(x)))
#            },
#            logical(1)))
# all(vapply(grid1,
#            function(r) {
#              all.equal(generalized_mean(r)(xna * a, na.rm = TRUE),
#                        generalized_mean(r)(xna, na.rm = TRUE) *
#                          generalized_mean(r)(a, factor_weights(r)(xna), na.rm = TRUE))
#            },
#            logical(1)))
# # Weighted case
# all(vapply(grid1,
#            function(r) {
#              all.equal(generalized_mean(r)(x * a, w),
#                        generalized_mean(r)(x, w) * generalized_mean(r)(a, factor_weights(r)(x, w)))
#            },
#            logical(1)))
# all(vapply(grid1,
#            function(r) {
#              all.equal(generalized_mean(r)(xna * a, w, na.rm = TRUE),
#                        generalized_mean(r)(xna, w, na.rm = TRUE) *
#                          generalized_mean(r)(a, factor_weights(r)(xna, w), na.rm = TRUE))
#            },
#            logical(1)))
