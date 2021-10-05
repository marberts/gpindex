\name{offset_prices}
\alias{back_price}
\alias{base_price}
\alias{back_period}
\alias{base_period}

\title{Offset prices}

\description{
Utility functions to offset a vector of prices, computing either the price in the previous period (back price), or the price in the first period (base price). Useful when price information is stored in a table. (Works well for quantities, too.)
}

\usage{
back_price(x, period, product = gl(1, length(x)))

base_price(x, period, product = gl(1, length(x)))

back_period(period, product = gl(1, length(period)))

base_period(period, product = gl(1, length(period)))
}

\arguments{
\item{x}{An atomic vector of prices.}

\item{period}{A factor, or something that can be coerced into one, that gives the corresponding time period for each element in \code{x}. The ordering of time periods follows the levels of \code{period} to agree with \code{\link[=cut.Date]{cut()}}.}

\item{product}{A factor, or something that can be coerced into one, that gives the corresponding product identifier for each element in \code{x}. The default is to assume that all prices are for the same product.}
}

\value{
\code{back_price()} and \code{base_price()} return an offset copy of \code{x}. With \code{back_price()}, for all periods after the first, the resulting vector gives the value of \code{x} for the corresponding product in the previous period. For the first time period, the resulting vector is the same as \code{x}. With \code{base_price()}, the resulting vector gives the value of \code{x} for the corresponding product in the first period.

\code{back_period()} and \code{base_period()} return a numeric vector of indices for the back/base periods.
}

\note{
By definition, there must be at most one price for each product in each time period to determine a back price. If multiple prices correspond to a period-product pair, then the back price at a point in time is always the first price for that product in the previous period.
}

\seealso{
\code{\link{outliers}} for common methods to detect outliers for price relatives.
}

\examples{
dat <- data.frame(price = 1:4, 
                  product = c("a", "b", "a", "b"), 
                  period = c(1, 1, 2, 2))

with(dat, back_price(price, period, product))

# (Almost) identical to indexing with the back period

with(dat, price[back_period(period, product)])

# Identical to the price in the base period with only two periods

with(dat, base_price(price, period, product))

# Reorder time periods by setting the levels in 'period'

with(dat, back_price(price, factor(period, levels = 2:1), product))

# Calculate price relatives

with(dat, price / back_price(price, period, product))

# Warning is given if the same product has multiple prices 
# at any point in time

with(dat, price / back_price(price, period))
}