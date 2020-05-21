naics6d <- with(naics2017, code[level == 5 & substr(code, 1, 3) == "311"])
each <- sample(5:30, length(naics6d), TRUE)
naics <- unlist(lapply(seq_along(naics6d), function(i) rep(naics6d[i], each = each[i])))

quarter <- rep(seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "quarter"), each = length(naics))

means <- rep(sample(seq(0, 2, by = 0.2), length(naics6d), TRUE), 4)
each <- rep(each, 4)
price <- unlist(lapply(seq_len(4 * length(naics6d)), function(i) rlnorm(each[i], meanlog = means[i])))

price_data <- data.frame(pid = seq_along(naics), quarter, price = round(price, 2), naics, stringsAsFactors = FALSE)