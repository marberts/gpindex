library(gpindex)
library(IndexNumR)

set.seed(1441)

# Corner cases
all.equal(fisher_geks(integer(0), numeric(0), logical(0), character(0)),
          list())

tornqvist_geks(1:2, 1:2, letters[1:2], c(1, 1))

# Compare with the GEKSIndex from IndexNumR
dat <- data.frame(price = runif(65), quantity = rlnorm(65), period = 1:13, product = rep(1:5, each = 13))
dat <- dat[c(65:60, 3:59, 1:2), ]

all.equal(cumprod(as.numeric(unlist(with(dat, tornqvist_geks(price, quantity, period, product))))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", prodID = "product", window = 13)[2:13]))

test <- with(dat, fisher_geks(price, quantity, period, product, 11))
all.equal(cumprod(as.numeric(c(test[[1]], test[[2]][10], test[[3]][10]))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 11, splice = "movement")[2:13]))

all.equal(cumprod(as.numeric(unlist(with(dat, fisher_geks(price, quantity, period, product, n = 11))))), 
          {res <- as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 13)); res[3:13] / res[2]})

all.equal(cumprod(as.numeric(unlist(with(dat, fisher_geks(price, quantity, period, product, n = 10))))), 
          {res <- as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 13)); res[4:13] / res[3]})

all.equal(cumprod(as.numeric(unlist(with(dat, fisher_geks(price, quantity, period, product, 2))))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 2, splice = "movement")[-1]))

all.equal(cumprod(as.numeric(unlist(with(dat, geks(arithmetic_index("Walsh1"))(price, quantity, period, product))))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "walsh", prodID = "product", window = 13)[2:13]))

dat <- dat[-c(2:3, 7, 15, 64), ]

all.equal(cumprod(as.numeric(unlist(with(dat, geks(balanced(geometric_index("Tornqvist")))(price, quantity, period, product, na.rm = TRUE))))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", prodID = "product", window = 13)[2:13]))

all.equal(as.numeric(with(dat, geks(balanced(fisher_index))(price, quantity, period, product, na.rm = TRUE, n = 1))), 
          {res <- as.numeric(GEKSIndex(dat, "price", "quantity", "period", "fisher", prodID = "product", window = 13)); res[13] / res[12]})

all.equal(cumprod(as.numeric(unlist(with(dat, geks(balanced(arithmetic_index("Walsh1")))(price, quantity, period, product, 2, na.rm = TRUE))))), 
          as.numeric(GEKSIndex(dat, "price", "quantity", "period", "walsh", prodID = "product", window = 2, splice = "movement")[2:13]))

# Default arguments should work with quantity_index()
all.equal(with(dat, geks(balanced(fisher_index))(price, quantity, period, product, na.rm = TRUE)),
          with(dat, quantity_index(geks(balanced(fisher_index)))(period, p = quantity, product, na.rm = TRUE, q = price)))

all.equal(with(dat, geks(balanced(fisher_index))(price, quantity, period, product, na.rm = TRUE)),
          with(dat, geks(balanced(quantity_index(fisher_index)))(period, p = quantity, product, na.rm = TRUE, q = price)))
