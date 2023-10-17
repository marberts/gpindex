#---- Tests for aggregate.ind() method ----
library(piar)

set.seed(12345)

# Tests with a matched-sample index
ms_epr <- with(
  ms_prices, 
  elemental_index(price_relative(price, period, product),
                  period, business, contrib = TRUE, na.rm = TRUE)
)

ms_pias <- with(
  ms_weights,
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)

(ms_index <- aggregate(ms_epr, ms_pias, na.rm = TRUE))

unclass(ms_index)

as_index(ms_index)

# Check against matrix calculation
all.equal(as.matrix(ms_pias) %*% as.matrix(chain(ms_index[paste0("B", 1:5)])), 
          as.matrix(chain(ms_index[1:3, ])))

# Check adding up of lower-level indexes
all.equal(apply(as.matrix(chain(ms_index)[4:8, ]), 2, weighted.mean, weights(ms_pias)[[3]]),
          as.matrix(chain(ms_index))[1, ])

all.equal(apply(as.matrix(chain(ms_index)[2:3, ]), 2, weighted.mean, weights(ms_pias)[[2]]),
          as.matrix(chain(ms_index))[1, ])

# Re-aggregating the index shouldn't do anything
all.equal(as.matrix(aggregate(ms_index, ms_pias)), as.matrix(ms_index))

all.equal(aggregate(chain(ms_index), ms_pias), chain(ms_index))

all.equal(as.matrix(aggregate(ms_index, ms_pias, na.rm = TRUE)), as.matrix(ms_index))

# A two-step aggregation should give the same result
pias2 <- aggregation_structure(list(c(1, 1), c(11, 12)), weights(ms_pias)[[2]])

aggregate(ms_index, pias2)

# Aggregating only elementals should just add B5
ms_epr2 <- aggregate(ms_epr, aggregation_structure(list(ms_weights$business), ms_weights$weight))

all.equal(ms_epr, ms_epr2[1:4, ])
ms_epr2[5, ]

all.equal(contrib(ms_epr), contrib(ms_epr2))

# Re-arranging the index shouldn't do anything
s <- c(14, 16, 26, 28, 24, 29, 11, 32, 36, 2, 22, 34, 6, 7, 10, 17, 8, 27, 37, 1, 12, 33, 20, 3, 9, 40, 13, 4, 38, 23, 31, 15, 25, 39, 21, 30, 35, 19, 18, 5)
ms_epr <- with(
  ms_prices[s, ], 
  elemental_index(price_relative(price, period, product),
                  period, business, contrib = TRUE, na.rm = TRUE)
)

s <- c(5, 3, 4, 1, 2)
ms_pias <- with(
  ms_weights[s, ],
  aggregation_structure(c(expand_classification(classification), list(business)), weight)
)

all.equal(as.matrix(aggregate(ms_epr, ms_pias, na.rm = TRUE)[levels(ms_index), ]), as.matrix(ms_index))

# Stacking shouldn't do anything
all.equal(Reduce(stack, unstack(ms_index)), ms_index)

# Aggregated contributions should add up
all.equal(as.matrix(ms_index)[1, ], 
          colSums(contrib(ms_index), na.rm = TRUE) + 1)

# Check that weights are getting price updated correctly
apply(as.matrix(chain(ms_index)[4:8, ]), 2, `*`, ms_weights$weight)

weights(update(ms_pias, ms_index), ea_only = TRUE)

weights(update(ms_pias, ms_index, "202003"), ea_only = TRUE)

# Do the same tests but with a weird index
ms_epr <- with(
  ms_prices,
  elemental_index(price_relative(price, period, product), 
                  period, business, contrib = TRUE, r = 0.2)
)

ms_index <- aggregate(ms_epr, ms_pias, r = -1.7, na.rm = TRUE)

all.equal(as.matrix(aggregate(ms_index, ms_pias, r = -1.7, na.rm = TRUE)), as.matrix(ms_index))

all.equal(aggregate(chain(ms_index), ms_pias, r = -1.7), chain(ms_index))

all.equal(as.matrix(ms_index)[1, ], 
          colSums(contrib(ms_index), na.rm = TRUE) + 1)

all.equal(apply(as.matrix(chain(ms_index)[2:3, ]), 2, gpindex::generalized_mean(-1.7), weights(ms_pias)[[2]]),
          as.matrix(chain(ms_index))[1, ])

all.equal((as.matrix(ms_pias) %*% as.matrix(chain(ms_index[paste0("B", s)]))^(-1.7))^(1 /-1.7), 
          as.matrix(chain(ms_index[1:3, ])))

ms_index <- aggregate(ms_epr, ms_pias, r = -1.7)

all.equal(aggregate(ms_index, ms_pias, r = -1.7), ms_index)

all.equal(as.matrix(ms_index)[1, ], 
          colSums(contrib(ms_index)) + 1)

# Tests with a fixed-sample index
fs_epr <- with(
  fs_prices, 
  elemental_index(price_relative(price, period, business),
                  period, classification, w = weight, contrib = TRUE)
)

fs_pias <- with(
  fs_weights,
  aggregation_structure(expand_classification(classification), weight)
)

(fs_index <- aggregate(fs_epr, fs_pias, na.rm = TRUE))

unclass(fs_index)

# Re-aggregating the index shouldn't do anything
all.equal(as.matrix(aggregate(fs_index, fs_pias)), as.matrix(fs_index))

# Contributions should add up
all.equal(as.matrix(fs_index)[1, ], 
          colSums(contrib(fs_index), na.rm = TRUE) + 1)

# Check adding up of lower level indexes
all.equal(apply(as.matrix(chain(fs_index)[5:9, ]), 2, weighted.mean, weights(fs_pias)[[3]]),
          as.matrix(chain(fs_index))[1, ])

all.equal(apply(as.matrix(chain(fs_index)[2:4, ]), 2, weighted.mean, weights(fs_pias)[[2]]),
          as.matrix(chain(fs_index))[1, ])

# Non-missing indexes should be the same when missing values are not removed
fs_index2 <- aggregate(fs_epr, fs_pias)
as.matrix(fs_index2) - as.matrix(fs_index)

all.equal(as.matrix(fs_index2)["121", ], 
          colSums(contrib(fs_index2, "121"), na.rm = TRUE) + 1)

all.equal(as.matrix(fs_index2)["13", 1:3], 
          contrib(fs_index2, "13")[, 1:3] + 1)

# Tests with a fixed-base index
prices <- data.frame(price = 1:15, 
                     period = letters[1:3], 
                     product = rep(1:5, each = 3), 
                     ea = rep(c("f1", "f2"), c(6, 9)))
prices$pop_rel <- with(prices, price_relative(price, period, product))
prices$fx_rel <- with(prices, price / price[gpindex::base_period(period, product)])

pias <- aggregation_structure(list(c("1", "1"), c("f1", "f2")), 1:2)

epr_pop <- with(prices, elemental_index(pop_rel, period, ea))
epr_fx <- with(prices, elemental_index(fx_rel, period, ea, chain = FALSE))

index_pop <- aggregate(epr_pop, pias)
index_fx <- aggregate(epr_fx, pias)

# Chained calculation and fixed-base calculation should be the same
all.equal(index_fx, chain(index_pop))
all.equal(chain(index_pop[, -1], as.matrix(index_fx[, 1])), index_fx[, -1])

# Should work for a non-arithmetic index
all.equal(chain(aggregate(epr_pop, pias, r = 3)), aggregate(epr_fx, pias, r = 3))

# Tests for methods
s1 <- merge(ms_index, fs_epr)
s1
is_aggregate_index(s1)
levels(s1)
time(s1)
contrib(s1, "111")

s2 <- ms_index[, 3:4]
s2$r <- s2$pias <- NULL
class(s2) <- "ind"

all.equal(stack(ms_index[, 1:2], s2), ms_index)

all.equal(stack(s2, ms_index[, 1:2]), ms_index[, c(3:4, 1:2)])

all.equal(stack(index_pop[-1, 1], index_pop[-1, letters[2:3]]), epr_pop)

all.equal(unstack(index_fx), list(index_fx[, 1], index_fx[, "b"], index_fx[, c(FALSE, FALSE, TRUE)]))
