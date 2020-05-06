\name{index weights}
\alias{index_weights}

\title{Calculate the weights for a variety of price indices}

\description{
Calculate the weights for a variety of price indices using information on prices and quantites
}

\usage{
index_weights(p0, p1, q0, q1, type, name)
}

\arguments{
\item{p0}{Base-period prices.}

\item{p1}{Current-period prices.}

\item{q0}{Base-period quantities.}

\item{q1}{Current-period quantities}

\item{type}{The type of index. One of "arithmetic", "geometric", "harmonic"}

\item{name}{The name of the index. See details.}
}

\value{
A numeric vector, the same length as \code{p0}.
}

\details{
To do
}

\references{
Balk, B. M. (2008). Price and Quantity Indices. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). Consumer Price Index Manual: Theory and Practice. International Monetary Fund.
}

\seealso{
See \code{\link[gpindex]{generalized_mean}} for the generalized mean.
}

\examples{
# Do to

}
