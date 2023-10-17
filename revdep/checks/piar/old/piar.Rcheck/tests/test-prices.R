#---- Compare results from piar with known values ----
library(piar)

wd <- getwd()

# Matched sample index
ms_cyg <- read.csv(file.path(wd, "ms-prices.csv"))

# Replicate the calculation with piar
pias <- with(
  ms_weights, 
  aggregation_structure(c(expand_classification(classification), list(business)), 
                        weight)
)

sp <- with(ms_prices, shadow_price(price, period, product, business, pias))

rel <- with(ms_prices, price_relative(sp, period, product))

epr <- with(ms_prices, elemental_index(rel, period, business, na.rm = TRUE))

index <- aggregate(epr, pias, na.rm = TRUE)

all.equal(as.numeric(as.matrix(chain(index))), ms_cyg$index / 100)

# Fixed sample index
fs_cyg <- read.csv(file.path(wd, "fs-prices.csv"))

# Reallocate the weights
weights <- fs_prices[1:11, c(2:3, 5)]
weights$weight <- with(
  weights, 
  ave(weight, classification, FUN = gpindex::scale_weights) * 
    fs_weights$weight[match(classification, fs_weights$classification)]
)

# Replicate with piar
pias <- with(
  weights, 
  aggregation_structure(c(expand_classification(classification), list(business)), 
                        weight)
)

rel <- with(fs_prices, price_relative(price, period, business))

epr <- with(fs_prices, elemental_index(rel, period, business))

index <- aggregate(epr, pias, na.rm = TRUE)

all.equal(as.numeric(as.matrix(chain(index)[1:9, ])), fs_cyg$index / 100)
