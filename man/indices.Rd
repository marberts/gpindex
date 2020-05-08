\name{price indices}
\alias{index_arithmetic}
\alias{index_geometric}
\alias{index_harmonic}
\alias{index_fisher}
\alias{index_hlp}
\alias{index_lm}
\alias{index_cswd}
\alias{index_cswdb}
\alias{index_bw}

\title{Calculate a variety of price indices}

\description{
Calculate a variety of price indices using information on prices and quantites.
}

\usage{
index_arithmetic(p0, p1, q0, q1, pb, qb, type, na.rm)

index_geometric(p0, p1, q0, q1, pb, qb, type, na.rm)

index_harmonic(p0, p1, q0, q1, pb, qb, type, na.rm)

index_fisher(p0, p1, q0, q1, na.rm)

index_hlp(p0, p1, q0, q1, na.rm)

index_lm(p0, p1, q0, q1, elasticity, na.rm)

index_cswd(p0, p1, na.rm)

index_cswdb(p0, p1, q0, q1, na.rm)

index_bw(p0, p1, na.rm)
}

\arguments{
\item{p0}{Base-period prices.}

\item{p1}{Current-period prices.}

\item{q0}{Base-period quantities.}

\item{q1}{Current-period quantities.}

\item{pb}{Period-b prices for the Lowe/Young index.}

\item{qb}{Period-b quantities for the Lowe/Young index.}

\item{elasticity}{The elasticity of substitution for the Lloyd-Moulton index.}

\item{type}{The name of the index. See details.}

\item{na.rm}{Should missing values be removed?}
}

\value{
A numeric value.
}

\details{
The functions for the arithmetic, geometric, harmonic, and Lloyd-Moulton indices are just convient wrappers for \code{generalized_mean(p1 / p0, index_weights(p0, p1, q0, q1))}. Together, the arithmetic, geometric, and harmonic index functions can calculate the following indices.
\itemize{
\item Carli
\item Dutot
\item Laspeyres
\item Paasche
\item Palgrave
\item Unnamed index (arithmetic analog of the Fisher)
\item Drobish
\item Walsh-I (arithmetic Walsh)
\item Marshall-Edgeworth
\item Geary-Khamis
\item Lowe
\item Young
\item Jevons
\item Geometric Laspeyres
\item Geometric Paasche
\item Geometric Young
\item Tornqvist
\item Sato-Vartia / Vartia-II
\item Walsh-II (geometric Walsh)
\item Coggeshall (equally weighted harmonic index)
\item Harmonic Laspeyres
}
In addition to these generalized indices, there are also functions for calculating a variety of non-generalized indices. The Fisher index is the geometric mean of the arithmetic Laspeyres and Paasche indices; the Harmonic Laspeyres Paasche index is the harmonic analog of the Fisher index. The Carruthers-Sellwood-Ward-Dalen and Carruthers-Sellwood-Ward-Dalen-Balk indices are sample analogs of the Fisher index; the Balk-Walsh index is the sample analog of the Walsh index. 

Naming for the indices follows the CPI/PPI manual first, then Balk (2008) for indices not listed (or not named) in the CPI/PPI manual.

In many cases these functions are slower than the direct calculation, mostly because special attention is paid to making sure NAs are correctly handled to remove missing price relatives when \code{na.rm = TRUE}.
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
# Most of these indices can be calculated by combining the 
# appropriate weights with the correct type of mean

p0 <- 1:5
p1 <- 2:6
q0 <- 1:5

index_geometric(p0, p1, q0, type = "Laspeyres")
geometric_mean(p1 / p0, index_weights(p0, p1, q0, type = "Laspeyres"))

# If only weights are available (no quantity information), then use one of the generalized means

geometric_mean(p1 / p0, c(0.1, 0.2, 0.4, 0.15, 0.15))

# NAs get special treatment

p0 <- c(1:4, NA)
p1 <- 2:6
q0 <- 1:5

# Drops the last price relative

index_arithmetic(p0, p1, q0, type = "Laspeyres", na.rm = TRUE)

# Only drops the last period-0 price

sum(p1 * q0, na.rm = TRUE) / sum(p0 * q0, na.rm = TRUE)


}
