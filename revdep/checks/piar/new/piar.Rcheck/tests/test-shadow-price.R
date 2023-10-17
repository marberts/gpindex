#---- Tests for shadow_price() ----
library(piar)

# Make an aggregation structure for the imputations
pias <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(business)),
                        weight)
)

# Length 0 prices return a length 0 output of the same type
shadow_price(integer(0), integer(0), integer(0), integer(0), pias)

# Imputing shadow prices does nothing
(sp <- with(ms_prices, shadow_price(price, period, product, business, pias)))

all.equal(sp, with(ms_prices, shadow_price(sp, period, product, business, pias)))

# First period with missing values gives the same result as ignoring them
# EPR without shadow prices
epr <- with(
  ms_prices, 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)
# EPR with shadow prices
epr2 <- with(
  ms_prices, 
  elemental_index(price_relative(sp, period, product), period, business, na.rm = TRUE)
)

all.equal(epr[c(1, 3:4), 1:3], epr2[c(1, 3:4), 1:3]) # B2 is imputed from above the EA level

all.equal(aggregate(epr, pias, na.rm = TRUE)[, 1:2], 
          aggregate(epr2, pias, na.rm = TRUE)[, 1:2])

# No imputation should happen if the pias doesn't line up with the elemental aggregates
# Append a '1' to each business label to make a garbage pias
pias2 <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(paste0(business, 1))), 
                        weight)
)

all.equal(ms_prices$price,
          with(ms_prices, shadow_price(price, period, product, business, pias2)))

# Jumbling prices does nothing
set.seed(4321)

jumble <- sample(nrow(ms_prices))
ms_prices <- ms_prices[jumble, ]
all.equal(with(ms_prices, shadow_price(price, period, product, business, pias)),
          sp[jumble])
