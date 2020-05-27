\name{quote contributions}
\alias{contribution}
\alias{contribution_geometric}
\alias{contribution_harmonic}
\alias{contribution_fisher}

\title{Quote contributions}

\description{
Calculate the additive decomposition of an index to get the contribution of each price relative.
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

\item{type}{The name of the index. See \code{\link[gpindex]{index}}.}

\item{na.rm}{Should missing values be removed?}
}

\value{
A numeric vector, the same length as \code{p1}.
}

\details{
For a given set of weights \code{w}, the additive contribution that each price relative (quote) \code{p1 / p0} has towards the value of a generalized price index can be calculated as 

\code{weights_change(p1 / p0, w, r, 1) / sum(weights_change(p1 / p0, w, r, 1)) * p1 / p0}. 

The \code{contribution_geometric} and \code{contribution_harmonic} functions simply call this, calculating the weights with \code{\link[gpindex]{index_weights}}.

The Fisher index is not a generalized index, but nonetheless has an additive decomposition (Balk, 2008, Section 4.2.2) that can be calculated with \code{contribution_fisher}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{weights_change}} can be used to calculate more general types of decompositions.
}

\examples{
# Make some data

p0 <- price6[1, ]
p1 <- price6[2, ]
q0 <- quantity6[1, ]
q1 <- quantity6[2, ]

# Additive decomposition of the Tornqvist index

(contrib <- contribution_geometric(p1, p0, q1, q0, type = "Tornqvist"))
index_geometric(p1, p0, q1, q0, type = "Tornqvist")
sum(contrib)

# Additive decomposition of the Fisher index

(contrib <- contribution_fisher(p1, p0, q1, q0))
index_fisher(p1, p0, q1, q0)
sum(contrib)

# Can be done manually if only weights are known

w <- runif(6)
contrib <- weights_h2a(p1 / p0, w)
(contrib <- contrib / sum(contrib) * p1 / p0)
mean_harmonic(p1 / p0, w)
sum(contrib)

# Manual approaches can be used to make a multiplicative decomposition

contrib <- weights_h2g(p1 / p0, w)
(contrib <- (p1 / p0) ^ (contrib / sum(contrib)))
prod(contrib)
}
