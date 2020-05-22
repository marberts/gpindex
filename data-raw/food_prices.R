naics6d <- with(naics2017, code[level == 5 & substr(code, 1, 3) == "311"])
each <- sample(5:30, length(naics6d), TRUE)
naics <- unlist(lapply(seq_along(naics6d), function(i) rep(naics6d[i], each = each[i])))

quarter <- rep(seq(as.Date("2020-04-01"), as.Date("2020-12-01"), by = "quarter"), each = length(naics))

max <- rep(sample(seq(1, 2, by = 0.1), length(naics6d), TRUE), 4)
each <- rep(each, 4)
price <- unlist(lapply(seq_len(4 * length(naics6d)), function(i) round(runif(each[i], 0.5, max[i]), 2)))

food_prices <- data.frame(pid = seq_along(naics), 
                          quarter, 
                          price_current = price[-seq_along(naics)], 
                          price_previous = price[seq_len(3 * length(naics))],
                          naics, 
                          stringsAsFactors = FALSE)
