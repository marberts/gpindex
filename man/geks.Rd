\name{geks}
\alias{geks}
\alias{tornqvist_geks}
\alias{fisher_geks}

\title{
GEKS index
}

\description{
Calculate an inter-temporal GEKS price index over a rolling window, as described in chapter 7 of Balk (2008), by Ivancic et al. (2011), and in chapter 10 of the CPI manual (2020).
}

\usage{
## Function factory
geks(f)

## Special cases
tornqvist_geks(p, q, period, product, 
               window = nlevels(period), n = window - 1, na.rm = FALSE)

fisher_geks(p, q, period, product, 
            window = nlevels(period), n = window - 1, na.rm = FALSE)
}

\arguments{
\item{f}{A \code{\link{price_index}} function that uses information on both base and current-period prices and quantities, and satisfies the time-reversal test. Usually a Törnqvist, Fisher, or Walsh index.}
\item{p}{A numeric vector of prices, the same length as \code{q}.}
\item{q}{A numeric vector of quantities, the same length as \code{p}.}
\item{period}{A factor, or something that can be coerced into one, that gives the corresponding time period for each element in \code{p} and \code{q}. The ordering of time periods follows the levels of \code{period} to agree with \code{\link[=cut.Date]{cut()}}.}
\item{product}{A factor, or something that can be coerced into one, that gives the corresponding product identifier for each element in \code{p} and \code{q}.}
\item{window}{The length of the rolling window. The default is a window that encompasses all periods in \code{period}. Values that are neither integers nor length 1 are silently truncated to a length 1 integer.}
\item{n}{A number giving the length of the index series for each window, starting from the end of the window. For example, if there are 13 periods in \code{window}, setting \code{n = 1} gives the index for period 13. The default gives an index for each period in \code{window}. Values that are neither integers nor length 1 are silently truncated to a length 1 integer.}
\item{na.rm}{Should missing values for \code{p} and \code{q} be removed when calculating the index? By default missing values will return a missing value for the index.}
}

\value{
\code{geks()} returns a function:

\preformatted{
function(p, q, period, product, 
         window = nlevels(period), n = window - 1, na.rm = FALSE){...}
}

This calculates a period-over-period GEKS index with the desired index-number formula, returning a list for each window with a named-numeric vector of index values.

\code{tornqvist_geks()} and \code{fisher_geks()} both return a list with a named numeric vector giving the value of the respective period-over-period GEKS index for each window.
}

\note{
Like \code{\link[=back_period]{back_period}}, if multiple prices correspond to a period-product pair, then the back price at a point in time is always the first price for that product in the previous period. Unlike a bilateral index, however, duplicated period-product pairs can have more subtle implications for a multilateral index.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

Ivancic, L., Diewert, W. E., and Fox, K. J. (2011). Scanner data, time aggregation and the construction of price indexes. \emph{Journal of Econometrics}, 161(1): 24--35.
}

\seealso{
\code{\link{price_index}} for price-index functions that can be used in \code{geks()}.
}

\examples{
price <- 1:6
quantity <- 6:1
period <- rep(1:3, 2)
product <- rep(letters[1:2], each = 3)

tornqvist_geks(price, quantity, period, product)

tornqvist_geks(price, quantity, period, product, window = 2)

# Missing data

quantity[2] <- NA

# Use all non-missing data

fisher_geks(price, quantity, period, product, na.rm = TRUE)

# Remove records with any missing data

fg <- geks(balanced(fisher_index))
fg(price, quantity, period, product, na.rm = TRUE)
}
