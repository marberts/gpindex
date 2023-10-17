## -----------------------------------------------------------------------------
options(stringsAsFactors = FALSE)

library(piar)

head(ms_prices)

ms_weights

## -----------------------------------------------------------------------------
relative <- with(ms_prices, price_relative(price, period, product))

(ms_epr <- with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE)))

## -----------------------------------------------------------------------------
ms_epr[, "202004"]
ms_epr["B1", ]

## -----------------------------------------------------------------------------
hierarchy <- with(ms_weights, c(expand_classification(classification), list(business)))

pias <- aggregation_structure(hierarchy, ms_weights$weight)

## -----------------------------------------------------------------------------
(ms_index <- aggregate(ms_epr, pias, na.rm = TRUE))

## -----------------------------------------------------------------------------
(ms_index_chained <- chain(ms_index))

## -----------------------------------------------------------------------------
t(apply(as.matrix(ms_index), 1, cumprod))

## -----------------------------------------------------------------------------
rebase(ms_index_chained, ms_index_chained[, "202004"])

## -----------------------------------------------------------------------------
rebase(ms_index_chained, rowMeans(as.matrix(ms_index_chained)[, c("202003", "202004")]))

## -----------------------------------------------------------------------------
(ms_weights <- transform(ms_weights, stratum = c("TS", "TA", "TS", "TS", "TS")))

## -----------------------------------------------------------------------------
(classification_sps <- with(ms_weights, paste0(classification, stratum)))

## -----------------------------------------------------------------------------
(classification_sps <- expand_classification(classification_sps, width = c(1, 1, 2)))
pias_sps <- with(
  ms_weights, 
  aggregation_structure(c(classification_sps, list(business)), weight)
)

## -----------------------------------------------------------------------------
aggregate(ms_epr, pias_sps, na.rm = TRUE)

## -----------------------------------------------------------------------------
ms_epr2 <- ms_epr
ms_epr2["B2", 2:3] <- 1
ms_epr2

## -----------------------------------------------------------------------------
aggregate(ms_epr2, pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE, r = 1))

## -----------------------------------------------------------------------------
with(ms_prices, elemental_index(relative, period, business, na.rm = TRUE, r = -1))

## -----------------------------------------------------------------------------
ms_prices2 <- transform(ms_prices, quantity = 10 - price)

## -----------------------------------------------------------------------------
library(gpindex)

tw <- grouped(index_weights("Tornqvist"))

ms_prices2[c("back_price", "back_quantity")] <- 
  ms_prices2[back_period(ms_prices2$period, ms_prices2$product), c("price", "quantity")]

ms_prices2 <- na.omit(ms_prices2) # can't have NAs for Tornqvist weights

ms_prices2$weight <- with(
  ms_prices2,
  tw(price, back_price, quantity, back_quantity, group = interaction(period, business))
)

## -----------------------------------------------------------------------------
with(ms_prices2, elemental_index(price / back_price, period, business, weight))

## -----------------------------------------------------------------------------
ms_epr <- with(ms_prices, elemental_index(relative, period, business, contrib = TRUE, na.rm = TRUE))

## -----------------------------------------------------------------------------
contrib(ms_epr)

## -----------------------------------------------------------------------------
ms_prices1 <- subset(ms_prices, period <= "202003")
ms_prices2 <- subset(ms_prices, period >= "202003")

## -----------------------------------------------------------------------------
ms_epr1 <- with(
  ms_prices1, 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)

(ms_index1 <- aggregate(ms_epr1, pias, na.rm = TRUE))

## -----------------------------------------------------------------------------
ms_epr2 <- with(
  subset(transform(ms_prices2, rel = price_relative(price, period, product)), period > "202003"),
  elemental_index(rel, period, business, na.rm = TRUE)
)

## -----------------------------------------------------------------------------
(ms_index2 <- aggregate(ms_epr2, update(pias, ms_index1), na.rm = TRUE))

## -----------------------------------------------------------------------------
chain(stack(ms_index1, ms_index2))

## -----------------------------------------------------------------------------
(ms_epr2 <- with(
  ms_prices, 
  elemental_index(price_relative(carry_forward(price, period, product), period, product), 
                  period, business, na.rm = TRUE)
)
)

## -----------------------------------------------------------------------------
(ms_index <- aggregate(ms_epr2, pias, na.rm = TRUE))

## -----------------------------------------------------------------------------
ms_prices1 <- subset(ms_prices, business %in% c("B1", "B2", "B3"))
ms_prices2 <- subset(ms_prices, business == "B4")

## -----------------------------------------------------------------------------
ms_epr1 <- with(
  ms_prices1, 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)
ms_epr1
ms_epr2 <- with(
  transform(ms_prices2, period = factor(period, levels = time(ms_epr1))), 
  elemental_index(price_relative(price, period, product), period, business, na.rm = TRUE)
)
ms_epr2

## -----------------------------------------------------------------------------
aggregate(merge(ms_epr1, ms_epr2), pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
ms_prices2 <- subset(
  as.data.frame(aggregate(ms_epr, pias, na.rm = TRUE)),
  level %in% c("B4", "B5")
)
ms_prices2

## -----------------------------------------------------------------------------
ms_epr2 <- with(ms_prices2, elemental_index(value, period, level))
aggregate(merge(ms_epr1, ms_epr2), pias, na.rm = TRUE)

## -----------------------------------------------------------------------------
weights <- data.frame(period = rep(c("202001", "202002", "202003", "202004"), each = 5),
                      classification = ms_weights$classification,
                      weight = 1:20)
head(weights)

## -----------------------------------------------------------------------------
(ms_epr <- unstack(ms_epr))

## -----------------------------------------------------------------------------
pias <- with(
  weights, 
  Map(aggregation_structure, 
      list(hierarchy), 
      split(weight, period))
)

## -----------------------------------------------------------------------------
(paasche <- Reduce(stack, Map(aggregate, ms_epr, pias, na.rm = TRUE, r = -1)))

## -----------------------------------------------------------------------------
laspeyres <- Reduce(stack, Map(aggregate, ms_epr, pias[c(1, 1, 2, 3)], na.rm = TRUE))
sqrt(as.matrix(laspeyres) * as.matrix(paasche))

