\name{price indices}
\alias{index}
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
Calculate a variety of price indices using information on prices and quantities.
}

\usage{
index_arithmetic(p1, p0, q1, q0, pb, qb, type, na.rm)

index_geometric(p1, p0, q1, q0, pb, qb, type, na.rm)

index_harmonic(p1, p0, q1, q0, pb, qb, type, na.rm)

index_fisher(p1, p0, q1, q0, na.rm)

index_hlp(p1, p0, q1, q0, na.rm)

index_lm(p1, p0, q1, q0, elasticity, na.rm)

index_cswd(p1, p0, na.rm)

index_cswdb(p1, p0, q1, q0, na.rm)

index_bw(p1, p0, na.rm)
}

\arguments{
\item{p1}{Current-period prices.}

\item{p0}{Base-period prices.}

\item{q1}{Current-period quantities.}

\item{q0}{Base-period quantities.}

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
The functions for the arithmetic, geometric, harmonic, and Lloyd-Moulton indices are just convenient wrappers for \code{mean_generalized(p1 / p0, index_weights(p1, p0, q1, q0))}. Together, the arithmetic, geometric, and harmonic index functions can calculate the following indices.
\itemize{
\item \bold{Arithmetic indices}
\item Carli
\item Dutot
\item Laspeyres
\item Palgrave
\item Unnamed index (arithmetic analog of the Fisher)
\item Drobish
\item Walsh-I (arithmetic Walsh)
\item Marshall-Edgeworth
\item Geary-Khamis
\item Lowe
\item Young
\item \bold{Geometric indices}
\item Jevons
\item Geometric Laspeyres
\item Geometric Paasche
\item Geometric Young
\item Tornqvist
\item Montgomery-Vartia / Vartia-I
\item Sato-Vartia / Vartia-II
\item Walsh-II (geometric Walsh)
\item \bold{Harmonic indices}
\item Coggeshall (equally weighted harmonic index)
\item Paasche
\item Harmonic Laspeyres
}
In addition to these generalized indices, there are also functions for calculating a variety of non-generalized indices. The Fisher index is the geometric mean of the arithmetic Laspeyres and Paasche indices; the Harmonic Laspeyres Paasche index is the harmonic analog of the Fisher index. The Carruthers-Sellwood-Ward-Dalen and Carruthers-Sellwood-Ward-Dalen-Balk indices are sample analogs of the Fisher index; the Balk-Walsh index is the sample analog of the Walsh index. 

Naming for the indices follows the CPI/PPI manual first, then Balk (2008) for indices not listed (or not named) in the CPI/PPI manual.

Dealing with missing values when making a price index is cumbersome, and best avoided. As there are different approaches for dealing with missing values in a price index, missing values should be dealt with prior to calculating the index. The approach taken here when \code{na.rm = TRUE} is to remove price relatives with missing information, either because of a missing price or a missing weight. Certain properties of an index-number formula may not work as expected with missing values, however, if there is ambiguity about how to remove missing values from the weights (as in e.g., a Tornqvist or Sato-Vartia index).
}

\references{
Balk, B. M. (2008). Price and Quantity Index Numbers. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). Consumer Price Index Manual: Theory and Practice. International Monetary Fund.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). Producer Price Index Manual: Theory and Practice. International Monetary Fund.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{index_weights}} calculates weights for the different types of indices. 

\code{\link[gpindex]{index_price_update}} can be used to price update weights for an arithmetic index. 

\code{\link[gpindex]{contribution}} calculates quote contributions.
}

\examples{
# Most of these indices can be calculated by combining the 
# appropriate weights with the correct type of mean

p0 <- 1:5
p1 <- 2:6
q0 <- 1:5

index_geometric(p1, p0, q0 = q0, type = "Laspeyres")
mean_geometric(p1 / p0, index_weights(p1, p0, q0 = q0, type = "Laspeyres"))

# If only weights are available (no quantity information), then use one of the generalized means

w <- c(0.1, 0.2, 0.4, 0.15, 0.15)
mean_geometric(p1 / p0, w)

# Chain an index by price updating the weights

p2 <- 3:7
index_arithmetic(p2, p0, q0 = q0, type = "Laspeyres")

I1 <- index_arithmetic(p1, p0, q0 = q0, type = "Laspeyres") 
w_pu <- index_price_update(p1 / p0, index_weights(p1, p0, q0 = q0, type = "Laspeyres"))
I2 <- mean_arithmetic(p2 / p1, w_pu)
I1 * I2

# Works for other types of indices, too

index_harmonic(p2, p0, q0 = q0, type = "Laspeyres")

I1 <- index_harmonic(p1, p0, q0 = q0, type = "Laspeyres") 
w_pu <- weights_factor(p1 / p0, index_weights(p1, p0, q0 = q0, type = "Laspeyres"), r = -1)
I2 <- mean_harmonic(p2 / p1, w_pu)
I1 * I2

# NAs get special treatment

p0 <- c(1:4, NA)
p1 <- 2:6
q0 <- 1:5

# Drops the last price relative

index_arithmetic(p1, p0, q0 = q0, type = "Laspeyres", na.rm = TRUE)

# Only drops the last period-0 price

sum(p1 * q0, na.rm = TRUE) / sum(p0 * q0, na.rm = TRUE)

}
