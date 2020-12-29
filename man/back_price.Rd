\name{back price}
\alias{back_price}

\title{Back price}

\description{
A simple utility function to compute the value in the previous period for a vector of prices.
}

\usage{
back_price(x, period, product = rep(1, length(x)))
}

\arguments{
\item{x}{A vector of prices.}

\item{period}{A factor, or something that can be coerced into one, that gives the corresponding time period for each element in \code{x}. The ordering of time periods follows the levels of \code{period} to agree with \code{\link[base:cut.Date]{cut()}}.}

\item{product}{A vector that gives the corresponding product identifier for each element in \code{x}. The default is to assume that all prices are for the same product.}
}

\value{
An offset copy of \code{x}. For all periods after the first, the resulting vector gives the value of \code{x} for the corresponding product in the previous period. For the first time period, the resulting vector is the same as \code{x}.
}

\note{
By definition, there must be at most one price for each product in each time period to determine a back price. If multiple prices correspond to a period-product pair, then the back price at a point in time is always the first price for that product in the previous period.
}

\examples{
dat <- data.frame(price = 1:4, product = c("a", "b", "a", "b"), period = c(1, 1, 2, 2))

with(dat, back_price(price, period, product))

# Reorder time periods by setting the levels in 'period'

with(dat, back_price(price, factor(period, levels = 2:1), product))

# Calculate price relatives

with(dat, price / back_price(price, period, product))

# Warning is given if the same product has multiple prices at any point in time

with(dat, price / back_price(price, period))
}