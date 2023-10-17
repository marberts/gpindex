#---- Tests for aggregation_structure() and associated methods ----
library(piar)

# Build an improper pias
# Should give a warning about improper parent nodes
aggregation_structure(list(1:2, c(3, 3), c(4, 5)))

# A real-ish example
x1 <- c("1", "1", "1", "2")
x2 <- c("11", "11", "12", "21")
x3 <- c("111", "112", "121", "211")

# Test the print.pias() method
(agg1 <- aggregation_structure(list(x1, x2, x3)))

unclass(agg1)

# Using expand classification should give the same result
all.equal(agg1, aggregation_structure(expand_classification(x3)))

# Coercion methods
as.matrix(aggregation_structure(1))

as.data.frame(aggregation_structure(1))

all.equal(agg1, 
          with(as.data.frame(agg1), aggregation_structure(list(level1, level2, ea), weight)))

# 4-levels, out of order
y1 <- c(1, 1, 1, 1, 1, 1)
y2 <- c(11, 11, 11, 12, 12, 12)
y3 <- c(111, 111, 112, 121, 122, 122)
y4 <- c(1111, 1112, 1121, 1211, 1221, 1222)
ord <- c(1, 6, 2, 4, 5, 3)
agg2 <- aggregation_structure(list(y1[ord], y2[ord], y3[ord], y4[ord]), c(rep(1, 5), 2))

as.matrix(agg2)

as.data.frame(agg2)

# Calculate weights with the weights.pias() method
weights(agg1)

weights(agg1, ea_only = TRUE)

# Unequal weights
weights(aggregation_structure(list(x1, x2, x3), 1:4))

weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4)))

weights(aggregation_structure(list(x1, x2, x3), c(NA, 2:4)), na.rm = TRUE)

# Update with the update.pias() method
# Updating with a length-0 index should make the weights NA
epr <- elemental_index(integer(0))
index <- aggregate(epr, agg1)
all.equal(update(agg1, index)[-5], agg1[-5])
weights(agg1)
weights(update(agg1, index))

# Updating with an index that doesn't line up with the pias introduces NA weights
# These should carry up the aggregation structure
epr <- elemental_index(1, ea = "111")
index <- aggregate(epr, agg1)
weights(update(agg1, index))

# Accommodate a delimiter when expanding the classification by setting the width
(agg2 <- aggregation_structure(expand_classification(c("1.1.1", "1.1.2", "1.2.1"), 
                                                     c(2, 2, 1))))

unclass(agg2)

# Change start by setting the width
(agg3 <- aggregation_structure(expand_classification(c("1.1.1", "1.1.2", "1.2.1"), 
                                                     c(4, 1))))

unclass(agg3)

# Aggregation matrix
as.matrix(agg3)

as.matrix(aggregation_structure(1:3, 2))

as.matrix(aggregation_structure(list(letters[c(2, 1, 3)]), 1:3))
