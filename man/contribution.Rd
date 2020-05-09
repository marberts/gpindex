\name{quote contributions}
\alias{contribution_geometric}
\alias{contribution_harmonic}
\alias{contribution_fisher}

\title{Quote contributions}

\description{
Calculate the additive decomposition on an index.
}

\usage{
contribution_geometric(p1, p0, q1, q0, pb, qb, type, na.rm = FALSE)

contribution_harmonic(p1, p0, q1, q0, pb, qb, type, na.rm = FALSE)

contribution_fisher(p1, p0, q1, q0, na.rm = FALSE)
}

\arguments{
\item{p1}{Current-period prices.}

\item{p0}{Base-period prices.}

\item{q1}{Current-period quantities.}

\item{q0}{Base-period quantities.}

\item{pb}{Period-b prices for the Lowe/Young index.}

\item{qb}{Period-b quantities for the Lowe/Young index.}

\item{type}{The name of the index. See \code{\link[gpindex]{price_indices}}.}

\item{na.rm}{Should missing values be removed?}
}

\value{
A numeric vector, the same length as \code{p1}.
}

\details{
To do.
}

\references{
Balk, B. M. (2008). Price and Quantity Index Numbers. Cambridge University Press.
}

\seealso{
See \code{\link[gpindex]{generalized_mean}} for the generalized mean.
}

\examples{
# To do

}
