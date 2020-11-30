### R code from vignette source 'gpindex.Rnw'

###################################################
### code chunk number 1: gpindex.Rnw:49-55
###################################################
library(gpindex)
p1 <- price6[[2]]
p0 <- price6[[1]]
mean_arithmetic(p1 / p0) # Carli index
mean_geometric(p1 / p0) # Jevons index
mean_harmonic(p1 / p0, runif(6)) # Harmonic index with random weights


###################################################
### code chunk number 2: gpindex.Rnw:64-69
###################################################
q0 <- quantity6[[1]]
index_weights("Laspeyres")(p0, q0) # period-0 expenditure shares
mean_arithmetic(p1 / p0, index_weights("Laspeyres")(p0, q0)) # Laspeyres index
sum(p1 * q0) / sum(p0 * q0) # Same as the manual calculation
mean_geometric(p1 / p0, index_weights("Laspeyres")(p0, q0)) # Geometric Laspeyres index


###################################################
### code chunk number 3: gpindex.Rnw:78-80
###################################################
index_arithmetic("Laspeyres")(p1, p0, q0)
index_geometric("Laspeyres")(p1, p0, q0)


###################################################
### code chunk number 4: gpindex.Rnw:85-88
###################################################
q1 <- quantity6[[2]]
index_fisher(p1, p0, q1, q0) 
index_hlp(p1, p0, q1, q0) # Harmonic analog of the Fisher index


###################################################
### code chunk number 5: gpindex.Rnw:150-154
###################################################
mean_harmonic(p1 / p0)
weights_transmute(-1, 1)(p1 / p0)
mean_arithmetic(p1 / p0, weights_transmute(-1, 1)(p1 / p0))
mean_geometric(p1 / p0, weights_transmute(-1, 0)(p1 / p0))


###################################################
### code chunk number 6: gpindex.Rnw:159-162
###################################################
hybrid <- index_weights("HybridPaasche")(p0, q1)
transmuted <- weights_transmute(-1, 1)(p1 / p0, index_weights("Paasche")(p1, q1))
all.equal(weights_scale(hybrid), weights_scale(transmuted))


###################################################
### code chunk number 7: gpindex.Rnw:167-169
###################################################
contributions(-1)(p1 / p0)
all.equal(sum(contributions(-1)(p1 / p0)), mean_harmonic(p1 / p0) - 1)


###################################################
### code chunk number 8: gpindex.Rnw:193-199
###################################################
p2 <- price6[[3]]
weights <- index_weights("Laspeyres")(p0, q0)
mean_harmonic(p2 / p0, weights) # Harmonic Laspeyres index
mean_harmonic(p1 / p0, weights) * mean_harmonic(p2 / p1, weights) # Too small
mean_harmonic(p1 / p0, weights) * 
  mean_harmonic(p2 / p1, weights_factor(-1)(p1 / p0, weights))


