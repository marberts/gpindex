\name{price indices}
\alias{arithmetic_index}
\alias{geometric_index}
\alias{harmonic_index}
\alias{fisher_index}
\alias{hlp_index}
\alias{lloyd_moulton_index}

\title{Calculate a variety of price indices}

\description{
Calculate a variety of price indices using information on prices and quantites.
}

\usage{
arithmetic_index(p0, p1, q0, q1, pb, qb, type, na.rm)

geometric_index(p0, p1, q0, q1, pb, qb, type, na.rm)

harmonic_index(p0, p1, q0, q1, pb, qb, type, na.rm)

fisher_index(p0, p1, q0, q1, na.rm)

hlp_index(p0, p1, q0, q1, na.rm)

lloyd_moulton_index(p0, p1, q0, q1, elasticity, na.rm)
}

\arguments{
\item{p0}{Base-period prices.}

\item{p1}{Current-period prices.}

\item{q0}{Base-period quantities.}

\item{q1}{Current-period quantities.}

\item{pb}{Period-b prices.}

\item{qb}{Period-b quantities.}

\item{elasticity}{The elasticity of substitution for the Lloyd-Moulton index.}

\item{type}{The name of the index. See details.}

\item{na.rm}{Should missing values be removed?}
}

\value{
A numeric value.
}

\details{
Namming for the indices follows the CPI/PPI manual first, then Balk (2008) for indices not listed (or not named) in the CPI/PPI manual.
}

\references{
Balk, B. M. (2008). Price and Quantity Index Numbers. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). Consumer Price Index Manual: Theory and Practice. International Monetary Fund.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). Producer Price Index Manual: Theory and Practice. International Monetary Fund.
}

\seealso{
See \code{\link[gpindex]{generalized_mean}} for the generalized mean. \code{\link[gpindex]{index_weights}} calculates weights for the different types of indices.
}

\examples{
# Do to

}
