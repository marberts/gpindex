#---- Tests for elemental_index() and index methods ----
options(stringsAsFactors = FALSE)

library(piar)
library(gpindex)

set.seed(1234)

# Make indexes with some random data
dat <- data.frame(rel = replace(rlnorm(1e4), sample(1e4, 10), NA),
                  period = sample(letters, 1e4, TRUE),
                  ea = sample(1:5, 1e4, TRUE),
                  w1 = replace(rlnorm(1e4), sample(1e4, 10), NA),
                  w2 = runif(1e4))

epr1 <- with(
  dat, 
  elemental_index(rel, period, ea, contrib = TRUE)
)
epr2 <- with(
  dat, 
  elemental_index(rel, period, ea, r = -1, contrib = TRUE, na.rm = TRUE)
)

# Test a Fisher calculation
fw <- function(x, w1, w2) {
  v1 <- scale_weights(transmute_weights(1, 0)(x, w1))
  v2 <- scale_weights(transmute_weights(-1, 0)(x, w2))
  v1 + v2
}

dat2 <- na.omit(dat)

w <- with(dat2, grouped(fw)(rel, w1, w2, group = interaction(period, ea)))

epr3 <- with(
  dat2, 
  elemental_index(rel, period, ea, w, contrib = TRUE)
)

# Compare with an alternate implementation
epr11 <- aggregate(rel ~ as.character(ea) + period, dat, 
                   function(x) exp(weighted.mean(log(x))), 
                   na.action = na.pass)
epr22 <- aggregate(rel ~ as.character(ea) + period, dat, 
                   function(x) 1 / weighted.mean(1 / x),
                   na.action = na.omit)

all.equal(as.data.frame(epr1), epr11[c(2, 1, 3)], check.attributes = FALSE)
all.equal(as.data.frame(epr2), epr22[c(2, 1, 3)], check.attributes = FALSE)

# chain() should be the same as using apply()
all.equal(as.matrix(chain(epr1)), t(apply(as.matrix(epr1), 1, cumprod)))
all.equal(as.matrix(unchain(chain(epr2))), as.matrix(epr2)) # contrib won't be the same

# rebase() should be the same as division
all.equal(as.matrix(rebase(chain(epr1), 1:5)), as.matrix(chain(epr1)) / 1:5)

# Contributions should add up
all.equal(epr1$index, 
          lapply(epr1$contrib, function(x) sapply(x, sum) + 1))

all.equal(as.matrix(epr1)["5", ], colSums(contrib(epr1, "5")) + 1) # note the padding

all.equal(epr2$index, 
          lapply(epr2$contrib, function(x) sapply(x, sum, na.rm = TRUE) + 1))
all.equal(epr3$index, 
          lapply(epr3$contrib, function(x) sapply(x, sum) + 1))

# Compare Fisher index with the manual calculation
l <- with(dat2, elemental_index(rel, period, ea, w1, r = 1))
p <- with(dat2, elemental_index(rel, period, ea, w2, r = -1))
all.equal(sqrt(as.matrix(l) * as.matrix(p)), as.matrix(epr3))

# Should work for other kinds of superlative indexes
fw <- function(x, w1, w2) {
  v1 <- scale_weights(transmute_weights(1.5, 0)(x))
  v2 <- scale_weights(transmute_weights(-1.5, 0)(x, w2))
  v1 + v2
}

w <- with(dat2, grouped(fw)(rel, w1, w2, group = interaction(period, ea)))

sepr <- with(dat2, elemental_index(rel, period, ea, w))

l <- with(dat2, elemental_index(rel, period, ea, r = 1.5))
p <- with(dat2, elemental_index(rel, period, ea, w2, r = -1.5))
all.equal(sqrt(as.matrix(l) * as.matrix(p)), as.matrix(sepr))

# Test merge.ind() method
epr2 <- with(
  dat, 
  elemental_index(rel, period, paste0(1, ea), r = -1, contrib = TRUE, na.rm = TRUE)
)

epr3 <- merge(epr1, epr2)
all.equal(as.matrix(epr3), rbind(as.matrix(epr1), as.matrix(epr2)))
all.equal(epr3$index$a, sapply(epr3$contrib$a, sum, na.rm = TRUE) + 1)
all.equal(levels(epr3), as.character(c(1:5, 11:15)))
all.equal(time(epr3), letters)

# Test stack.ind() method
epr2 <- with(
  dat, 
  elemental_index(rel, toupper(period), ea, r = -1, contrib = TRUE, na.rm = TRUE)
)
epr3 <- stack(epr1, epr2)
all.equal(as.matrix(epr3), cbind(as.matrix(epr1), as.matrix(epr2)))
all.equal(epr3$index$A, sapply(epr3$contrib$A, sum, na.rm = TRUE) + 1)
all.equal(levels(epr3), as.character(1:5))
all.equal(time(epr3), c(letters, LETTERS))

# Stacking and unstacking are opposite operations
all.equal(epr1, Reduce(stack, unstack(epr1)))

# Coercion are opposite operations
all.equal(as_index(as.matrix(epr1)), as_index(as.data.frame(epr1)))
all.equal(chain(epr1), as_index(as.matrix(chain(epr1)), chain = FALSE))
all.equal(chain(epr1), as_index(as.data.frame(chain(epr1))[c(2, 1, 3)], c(2, 1, 3), chain = FALSE))

# Test mean.ind()
epr4 <- mean(epr1, window = 12)
all.equal(levels(epr4), levels(epr1))
time(epr4)
all.equal(as.matrix(epr4)[, 1], rowMeans(as.matrix(epr1)[, 1:12]))
all.equal(as.matrix(epr4)[, 2], rowMeans(as.matrix(epr1)[, 13:24]))
is_chainable_index(epr4)
is_chainable_index(mean(chain(epr1)))
epr4$contrib

w <- matrix(seq_len(5 * 26), 5)
all.equal(as.matrix(mean(epr1, w, window = 12))[, 1], 
          diag(as.matrix(epr1)[, 1:12] %*% apply(w[, 1:12], 1, scale_weights)), check.attributes = FALSE)
all.equal(as.matrix(mean(epr1, w, window = 12))[, 2], 
          diag(as.matrix(epr1)[, 13:24] %*% apply(w[, 13:24], 1, scale_weights)), check.attributes = FALSE)

# Test head/tail
all.equal(head(epr1), epr1)
all.equal(head(epr1, 2), epr1[1:2])
all.equal(head(epr1, c(-2, 2)), epr1[1:3, 1:2])
all.equal(head(epr1, c(NA, 2)), epr1[, 1:2])
all.equal(tail(epr1), epr1)
all.equal(tail(epr1, 2), epr1[4:5])
all.equal(tail(epr1, c(-2, 2)), epr1[3:5, 25:26])
all.equal(tail(epr1, c(NA, 2)), epr1[, 25:26])

# Toy example that can be easily verified
dat <- data.frame(rel = c(1:6, NA, 7, 8),
                  period = c(1, 1, 1, 1, 1, 2, 2, 2, 2),
                  ea = c("11", "11", "12", "12", "13", "11", "12", "11", "14"))

all.equal(as_index(dat, c("period", "ea", "rel")),
          with(dat[c(2, 4, 5, 7, 8, 9), ], elemental_index(rel, period, ea)))

(epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE)))
unclass(epr)
as.matrix(epr)
contrib(epr)
as.data.frame(epr)
unclass(epr[, 1])
unclass(epr[1, ])
chain(epr)$contrib
all.equal(chain(epr), rebase(chain(epr)))
all.equal(rebase(epr), epr)

epr2 <- with(dat, elemental_index(rel, period, ea))

all.equal(as.matrix(epr), as.matrix(epr2))
all.equal(epr2, with(as.data.frame(epr2), elemental_index(value, period, level)))
all.equal(chain(epr2), as_index(as.matrix(chain(epr2)), chain = FALSE))
all.equal(epr$levels, epr2$levels)
all.equal(epr$time, epr2$time)

contrib(epr2)

epr[] <- as.matrix(epr2)
all.equal(epr, epr2)
all.equal(contrib(epr), contrib(epr2))
all.equal(levels(epr), levels(epr2))
all.equal(time(epr), time(epr2))
is_chainable_index(epr)
is_chainable_index(epr2)

# It shouldn't be possible to make a non-numeric index
mat <- as.matrix(data.frame(a = as.character(1:5), b = 1:5))

epr <- as_index(mat)
is.numeric(as.matrix(epr))

epr[, "b"] <- as.character(1:5)
all.equal(epr, as_index(mat))

# Nor one without EA names
as_index(matrix(1:5, ncol = 5, dimnames = list("a", 1:5)))

# Test replacement method
epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE))

epr[, 1] <- 0
epr
epr$contrib

epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE))

epr[1:2, "2"] <- 0
epr
epr$contrib

epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE))

epr[c(T, F, F, T), -1] <- 0
epr
epr$contrib

epr <- with(dat, elemental_index(rel, period, ea, contrib = TRUE))

epr["11", ] <- 0
epr
epr$contrib

epr[-1, ] <- epr[1, ]
epr
epr$contrib
