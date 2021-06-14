### R code from vignette source 'gpindex.Rnw'

###################################################
### code chunk number 1: gpindex.Rnw:49-55
###################################################
library(gpindex)
p1 <- price6[[2]]
p0 <- price6[[1]]
arithmetic_mean(p1 / p0) # Carli index
geometric_mean(p1 / p0) # Jevons index
harmonic_mean(p1 / p0, 1:6) # Harmonic index with random weights


###################################################
### code chunk number 2: gpindex.Rnw:64-69
###################################################
q0 <- quantity6[[1]]
index_weights("Laspeyres")(p0, q0) # period-0 expenditure shares
arithmetic_mean(p1 / p0, index_weights("Laspeyres")(p0, q0)) # Laspeyres index
sum(p1 * q0) / sum(p0 * q0) # Same as the manual calculation
geometric_mean(p1 / p0, index_weights("Laspeyres")(p0, q0)) # Geometric Laspeyres index


###################################################
### code chunk number 3: gpindex.Rnw:78-80
###################################################
arithmetic_index("Laspeyres")(p1, p0, q0)
geometric_index("Laspeyres")(p1, p0, q0)


###################################################
### code chunk number 4: gpindex.Rnw:85-88
###################################################
q1 <- quantity6[[2]]
fisher_index(p1, p0, q1, q0) 
hlp_index(p1, p0, q1, q0) # Harmonic analog of the Fisher index


###################################################
### code chunk number 5: gpindex.Rnw:150-154
###################################################
harmonic_mean(p1 / p0)
transmute_weights(-1, 1)(p1 / p0)
arithmetic_mean(p1 / p0, transmute_weights(-1, 1)(p1 / p0))
geometric_mean(p1 / p0, transmute_weights(-1, 0)(p1 / p0))


###################################################
### code chunk number 6: gpindex.Rnw:159-162
###################################################
hybrid <- index_weights("HybridPaasche")(p0, q1)
transmuted <- transmute_weights(-1, 1)(p1 / p0, index_weights("Paasche")(p1, q1))
all.equal(scale_weights(hybrid), scale_weights(transmuted))


###################################################
### code chunk number 7: gpindex.Rnw:167-169
###################################################
contributions(-1)(p1 / p0)
all.equal(sum(contributions(-1)(p1 / p0)), harmonic_mean(p1 / p0) - 1)


###################################################
### code chunk number 8: gpindex.Rnw:193-199
###################################################
p2 <- price6[[3]]
weights <- index_weights("Laspeyres")(p0, q0)
harmonic_mean(p2 / p0, weights) # Harmonic Laspeyres index
harmonic_mean(p1 / p0, weights) * harmonic_mean(p2 / p1, weights) # Too small
harmonic_mean(p1 / p0, weights) * 
  harmonic_mean(p2 / p1, factor_weights(-1)(p1 / p0, weights))


