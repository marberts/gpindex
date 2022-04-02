\name{offset_prices}
\alias{back_period}
\alias{base_period}
\alias{back_price}
\alias{base_price}

\title{Offset prices}

\description{
Utility functions to offset vectors of prices and quantities. For each product, these compute either the index for the previous period (back period), or the index for the first period (base period). Useful when price information is stored in a table.
}

\usage{
back_period(period, product = gl(1, length(period)))

base_period(period, product = gl(1, length(period)))

back_price(x, period, product = gl(1, length(x))) # deprecated

base_price(x, period, product = gl(1, length(x))) # deprecated
}

\arguments{
\item{period}{A factor, or something that can be coerced into one, that gives the time period for each transaction. The ordering of time periods follows the levels of \code{period} to agree with \code{\link[=cut.Date]{cut()}}.}

\item{product}{A factor, or something that can be coerced into one, that gives the product identifier for each transaction. The default is to assume that all transactions are for the same product.}

\item{x}{An atomic vector of prices.}
}

\value{
\code{back_period()} and \code{base_period()} return a numeric vector of indices for the back/base periods. With \code{back_period()}, for all periods after the first, the resulting vector gives the location of the corresponding product in the previous period. The locations are unchanged for the first time period. With \code{base_period()}, the resulting vector gives the location of the corresponding product in the first period.

\code{back_price()} and \code{base_price()} return an offset copy of \code{x}. 
}

\note{
By definition, there must be at most one transaction for each product in each time period to determine a back period. If multiple transactions correspond to a period-product pair, then the back period at a point in time is always the first location for that product in the previous period.
}

\seealso{
\code{\link{outliers}} for common methods to detect outliers for price relatives.

\code{\link{generalized_mean}} for the generalized mean.
}

\examples{
df <- data.frame(price = 1:6, 
                 product = factor(c("a", "b")), 
                 period = factor(c(1, 1, 2, 2, 3, 3)))

with(df, back_period(period, product))

# Make period-over-period price relatives

with(df, price / price[back_period(period, product)])

# Make fixed-base price relatives

with(df, price / price[base_period(period, product)])

# Change the base period with relevel()

with(df, price / price[base_period(relevel(period, "2"), product)])

# Warning is given if the same product has multiple prices 
# at any point in time

with(df, back_period(period))
}
