pkgname <- "piar"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('piar')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("aggregation-structure")
### * aggregation-structure

flush(stderr()); flush(stdout())

### Name: aggregation_structure
### Title: Aggregation structure
### Aliases: aggregation_structure weights.pias update.pias as.matrix.pias
###   as.data.frame.pias expand_classification

### ** Examples

# A simple example
#            1
#      |-----+-----|
#      11          12
#  |---+---|       |
#  111     112     121

x1 <- c("1",   "1",   "1")
x2 <- c("11",  "11",  "12")
x3 <- c("111", "112", "121")

aggregation_structure(list(x1, x2, x3))

# The aggregation structure can also be made by expanding 'x3'

expand_classification(x3)

all.equal(aggregation_structure(list(x1, x2, x3)), 
          aggregation_structure(expand_classification(x3)))

# Unequal weights

aggregation_structure(list(x1, x2, x3), 1:3)
          
# Extract the weights

weights(aggregation_structure(list(x1, x2, x3)))

# Expanding more complex classifications
# ... if last 'digit' is either TA or TS

expand_classification(c("111TA", "112TA", "121TS"), width = c(1, 1, 1, 2))

# ... if first 'digit' is either 11 or 12

expand_classification(c("111", "112", "121"), width = c(2, 1))

# ...if there are delimiters in the classification (like COICOP)

expand_classification(c("01.1.1", "01.1.2", "01.2.1"), width = c(2, 2, 2))



cleanEx()
nameEx("chain")
### * chain

flush(stderr()); flush(stdout())

### Name: chain
### Title: Chain and rebase a price index
### Aliases: chain chain.default chain.ind unchain unchain.default
###   unchain.ind rebase rebase.default rebase.ind is_chainable_index
###   is_chain_index

### ** Examples

prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A simple period-over-period elemental index

(epr <- with(prices, elemental_index(rel, period, ea)))

# Make period 0 the fixed base period

chain(epr)

# Chaining and unchaining reverse each other

all.equal(epr, unchain(chain(epr)))

# Change the base period to period 2 (note the loss of information for period 0)

epr <- chain(epr)
rebase(epr, epr[, 2])



cleanEx()
nameEx("contrib")
### * contrib

flush(stderr()); flush(stdout())

### Name: contrib
### Title: Extract quote contributions
### Aliases: contrib contrib.ind

### ** Examples

prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

epr <- with(prices, elemental_index(rel, period, ea, contrib = TRUE))

pias <- aggregation_structure(list(c("top", "top", "top"), c("a", "b", "c")), 1:3)

index <- aggregate(epr, pias, na.rm = TRUE)

# Quote contributions for the top-level index
contrib(index)

# Calculate EA contributions for the chained index
gpindex::arithmetic_contributions(as.matrix(chain(index))[c("a", "b", "c"), 2], 
                                  weights(pias, ea_only = TRUE))



cleanEx()
nameEx("impute-prices")
### * impute-prices

flush(stderr()); flush(stdout())

### Name: impute_prices
### Title: Impute prices
### Aliases: carry_forward shadow_price

### ** Examples

prices <- data.frame(price = c(1:7, NA), period = rep(1:2, each = 4), 
                     product = 1:4, ea = rep(letters[1:2], 4))
                     
with(prices, carry_forward(price, period, product))

with(prices, shadow_price(price, period, product, ea))



cleanEx()
nameEx("price-indexes")
### * price-indexes

flush(stderr()); flush(stdout())

### Name: price_indexes
### Title: Price indexes
### Aliases: elemental_index elemental_index.default
###   elemental_index.numeric aggregate.ind vcov.agg_ind mean.ind merge.ind
###   stack.ind unstack.ind [.ind [<-.ind levels.ind time.ind start.ind
###   end.ind head.ind tail.ind summary.ind as.matrix.ind as.data.frame.ind
###   as_index as_index.default as_index.matrix as_index.data.frame
###   is_index is_aggregate_index

### ** Examples

prices <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[1:2], 4))

# A two-level aggregation structure

pias <- aggregation_structure(list(c("top", "top", "top"), c("a", "b", "c")), 1:3)

# Calculate Jevons elemental indexes

(epr <- with(prices, elemental_index(rel, period, ea)))

# Same as using lm() or tapply()

exp(coef(lm(log(rel) ~ ea:factor(period) - 1, prices)))

with(prices, t(tapply(rel, list(period, ea), gpindex::geometric_mean, na.rm = TRUE)))

# Extract the indexes like a matrix

epr["a", ]

epr[, 2]

epr[1, ] <- 1 # can be useful for doing specific imputations

# Aggregate (note the imputation for elemental index 'c')

(index <- aggregate(epr, pias, na.rm = TRUE))

# Aggregation can equivalently be done as matrix multiplication

as.matrix(pias) %*% as.matrix(chain(index[letters[1:3]]))

# Merge two indexes prior to aggregation

prices2 <- data.frame(rel = 1:8, period = rep(1:2, each = 4), ea = rep(letters[3:4], 4))
epr2 <- with(prices2, elemental_index(rel, period, ea))
aggregate(merge(epr, epr2), pias)

# Stack two indexes prior to aggregation

prices3 <- data.frame(rel = 1:8, period = rep(3:4, each = 4), ea = rep(letters[1:2], 4))
epr3 <- with(prices3, elemental_index(rel, period, ea))
aggregate(stack(epr, epr3), pias)

# Unstack does the reverse

all.equal(c(unstack(epr), unstack(epr3)), unstack(stack(epr, epr3)))

# Extract useful features of the index

head(index, 1)
tail(index, 3)
levels(index)
time(index)
start(index)
end(index)

summary(index)

# Turn the index into a data frame/matrix

as.data.frame(index)
as.matrix(index)

all.equal(as_index(as.data.frame(epr)), epr)
all.equal(as_index(as.matrix(epr)), epr)

# Calculate a CSWD index (same as the Jevons in this example) 
# as an arithmetic index by constructing appropriate weights

library(gpindex)

# A general function to calculate weights to turn the geometric
# mean of the arithmetic and harmonic mean (i.e., Fisher mean)
# into an arithmetic mean

fw <- grouped(nested_transmute(0, c(1, -1), 1))

with(
    prices, 
    elemental_index(rel, period, ea, fw(rel, group = interaction(period, ea)), r = 1)
)



cleanEx()
nameEx("price-relative")
### * price-relative

flush(stderr()); flush(stdout())

### Name: price_relative
### Title: Price relative
### Aliases: price_relative

### ** Examples

price_relative(1:6, rep(1:2, each = 3), rep(letters[1:3], 2))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
