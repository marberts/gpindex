library(gpindex)

# Make some data for the tests
price <- 1:10
id <- letters[c(5:1, 1:5)]
period <- rep(1:2, each = 5)
names(price) <- id

back_price(NULL, NULL, NULL)
back_price(1:4, rep(1, 4))
back_price(matrix(1:4), rep(1, 4))
back_price(1:4, factor(rep(1, 4), levels = 0:1))
back_price(1:4, 1:4) == c(1, 1, 2, 3)
base_price(1:4, 1:4) == c(1, 1, 1, 1)
back_price(1:4, factor(1:4, levels = 4:1))
back_price(price, period, id)
back_price(price, replace(period, 2, NA), id)
back_price(price[-1], period[-1], id[-1])
back_price(price, period, replace(id, 1, NA))
back_price(replace(price, 1, NA), period, id)
