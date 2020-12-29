price <- 1:10
id <- letters[c(5:1, 1:5)]
period <- rep(1:2, each = 5)
names(price) <- id

stopifnot(
  exprs = {
    length(back_price(NULL, NULL, NULL)) == 0
    back_price(1:4, rep(1, 4), 1:4) == 1:4
    base_price(1:4, rep(1, 4), 1:4) == 1:4
    identical(back_price(matrix(1:4), rep(1, 4), 1:4), matrix(1:4))
    is.na(back_price(1:4, factor(rep(1, 4), levels = 0:1), 1:4))
    back_price(1:4, 1:4, rep(1, 4)) == c(1, 1, 2, 3)
    base_price(1:4, 1:4, rep(1, 4)) == c(1, 1, 1, 1)
    back_price(1:4, 1:4) == c(1, 1, 2, 3)
    back_price(1:4, factor(1:4, levels = 4:1), rep(1, 4)) == c(2, 3, 4, 4)
    identical(price / back_price(price, period, id), 
              structure(c(1, 1, 1, 1, 1, 6:10 / 5:1), names = id))
    identical(price / back_price(price, replace(period, 2, NA), id), 
              structure(c(1, NA, 1, 1, 1, c(6:8, NA, 10) / 5:1), names = id))
    identical(back_price(price[-1], period[-1], id[-1]), 
              structure(c(2:5, 5:2, NA), names = id[-1]))
    identical(back_price(price, period, replace(id, 1, NA)), 
              structure(c(NA, 2:5, 5:2, NA), names = id))
    identical(back_price(replace(price, 1, NA), period, id), 
              structure(c(NA, 2:5, 5:2, NA), names = id))
  },
  local = getNamespace("gpindex")
)
