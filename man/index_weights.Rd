\name{index weights}
\alias{index_weights}

\title{Calculate the weights for a variety of price indices}

\description{
Calculate the weights for a variety of price indices using information on prices and quantites.
}

\usage{
index_weights(p0, p1, q0, q1, pb, qb, type, na.rm)
}

\arguments{
\item{p0}{Base-period prices.}

\item{p1}{Current-period prices.}

\item{q0}{Base-period quantities.}

\item{q1}{Current-period quantities.}

\item{pb}{Period-b prices.}

\item{qb}{Period-b quantities.}

\item{type}{The name of the index. See details.}

\item{na.rm}{Should missing values be removed?}
}

\value{
A numeric vector, the same length as \code{p0}.
}

\details{
Setting \code{na.rm = TRUE} simply removes missing values when scaling the weights so that they sum to 1. Any missing values will still appear in the return value to keep it the same length as the input values.
}

\references{
Balk, B. M. (2008). Price and Quantity Index Numbers. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). Consumer Price Index Manual: Theory and Practice. International Monetary Fund.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). Producer Price Index Manual: Theory and Practice. International Monetary Fund.
}

\seealso{
See \code{\link[gpindex]{generalized_mean}} for the generalized mean.
}

\examples{
# Do to

}
