\name{price indexes}
\alias{index}
\alias{index_arithmetic}
\alias{index_lowe}
\alias{index_young}
\alias{index_geometric}
\alias{index_harmonic}
\alias{index_fisher}
\alias{index_hlp}
\alias{index_lm}
\alias{index_cswd}
\alias{index_cswdb}
\alias{index_bw}

\title{Calculate a variety of price indexes}

\description{
Calculate a variety of price indexes using information on prices and quantities.
}

\usage{
index_arithmetic(p1, p0, q1, q0, type, na.rm)

index_lowe(p1, p0, qb, na.rm)

index_young(p1, p0, pb, qb, na.rm)

index_geometric(p1, p0, q1, q0, type, na.rm)

index_harmonic(p1, p0, q1, q0, type, na.rm)

index_fisher(p1, p0, q1, q0, na.rm)

index_hlp(p1, p0, q1, q0, na.rm)

index_lm(p1, p0, q0, elasticity, na.rm)

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
The functions for the arithmetic, geometric, harmonic, and Lloyd-Moulton indexes are just convenient wrappers for \code{mean_generalized(p1 / p0, index_weights(p1, p0, q1, q0))}. Together, the arithmetic, geometric, and harmonic index functions can calculate the following indexes.
\itemize{
\item \bold{Arithmetic indexes}
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
\item \bold{Geometric indexes}
\item Jevons
\item Geometric Laspeyres
\item Geometric Paasche
\item Tornqvist
\item Montgomery-Vartia / Vartia-I
\item Sato-Vartia / Vartia-II
\item Walsh-II (geometric Walsh)
\item \bold{Harmonic indexes}
\item Coggeshall (equally weighted harmonic index)
\item Paasche
\item Harmonic Laspeyres
}
In addition to these generalized indexes, there are also functions for calculating a variety of non-generalized indexes. The Fisher index is the geometric mean of the arithmetic Laspeyres and Paasche indexes; the Harmonic Laspeyres Paasche index is the harmonic analog of the Fisher index. The Carruthers-Sellwood-Ward-Dalen and Carruthers-Sellwood-Ward-Dalen-Balk indexes are sample analogs of the Fisher index; the Balk-Walsh index is the sample analog of the Walsh index. 

Naming for the indexes follows the CPI/PPI manual first, then Balk (2008) for indexes not listed (or not named) in the CPI/PPI manual.

Dealing with missing values when making a price index is cumbersome, and best avoided. As there are different approaches for dealing with missing values in a price index, missing values should be dealt with prior to calculating the index. The approach taken here when \code{na.rm = TRUE} is to remove price relatives with missing information, either because of a missing price or a missing weight. Certain properties of an index-number formula may not work as expected with missing values, however, if there is ambiguity about how to remove missing values from the weights (as in e.g., a Tornqvist or Sato-Vartia index).
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{index_weights}} calculates weights for the different types of indexes. 
}

\examples{
# Most of these indexes can be calculated by combining the 
# appropriate weights with the correct type of mean

p0 <- price6[[1]]
p1 <- price6[[2]]
q0 <- quantity6[[1]]
q1 <- quantity6[[1]]

index_geometric(p1, p0, q0 = q0, type = "Laspeyres")
mean_geometric(p1 / p0, index_weights(p1, p0, q0 = q0, type = "Laspeyres"))

# If only weights are available (no quantity information), then use one of the generalized means

w <- c(0.1, 0.2, 0.4, 0.15, 0.10, 0.05)
mean_geometric(p1 / p0, w)

# Chain an index by price updating the weights

p2 <- price6[[3]]
index_arithmetic(p2, p0, q0 = q0, type = "Laspeyres")

I1 <- index_arithmetic(p1, p0, q0 = q0, type = "Laspeyres") 
w_pu <- weights_factor(p1 / p0, index_weights(p1, p0, q0 = q0, type = "Laspeyres"), r = 1)
I2 <- mean_arithmetic(p2 / p1, w_pu)
I1 * I2

# Works for other types of indexes, too

index_harmonic(p2, p0, q0 = q0, type = "Laspeyres")

I1 <- index_harmonic(p1, p0, q0 = q0, type = "Laspeyres") 
w_pu <- weights_factor(p1 / p0, index_weights(p1, p0, q0 = q0, type = "Laspeyres"), r = -1)
I2 <- mean_harmonic(p2 / p1, w_pu)
I1 * I2

# Quote contribution for the Tornqvist index

(con <- weights_g2a(p1 / p0, index_weights(p1, p0, q1, q0, "Tornqvist")) * p1 / p0)

sum(con)
index_geometric(p1, p0, q1, q0, "Tornqvist")

# Quote contribution for the Fisher index
wl <- index_weights(p1, p0, q1, q0, "Laspeyres")
wp <- index_weights(p1, p0, q1, q0, "HybridPaasche")
wf <- weights_g2a(c(mean_arithmetic(p1 / p0, wl), mean_arithmetic(p1 / p0, wp)))
(con <- wf * (wl + wp) * p1 / p0)

sum(con)
index_fisher(p1, p0, q1, q0)

# NAs get special treatment

p0[6] <- NA

# Drops the last price relative

index_arithmetic(p1, p0, q0 = q0, type = "Laspeyres", na.rm = TRUE)

# Only drops the last period-0 price

sum(p1 * q0, na.rm = TRUE) / sum(p0 * q0, na.rm = TRUE)

}
