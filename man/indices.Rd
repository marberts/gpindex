\name{price indexes}
\alias{index}
\alias{index_arithmetic}
\alias{index_geometric}
\alias{index_harmonic}
\alias{index_laspeyres}
\alias{index_paasche}
\alias{index_lowe}
\alias{index_young}
\alias{index_fisher}
\alias{index_hlp}
\alias{index_lm}
\alias{index_cswd}
\alias{index_cswdb}
\alias{index_bw}
\alias{index_stuval}
\alias{index_weights}

\title{Calculate a variety of price indexes}

\description{
Calculate a variety of price indexes using information on prices and quantities.
}

\usage{
index_arithmetic(type)

index_geometric(type)

index_harmonic(type)

index_weights(type)

index_laspeyres(p1, p0, q0, na.rm = FALSE)

index_paasche(p1, p0, q1, na.rm = FALSE)

index_lowe(p1, p0, qb, na.rm = FALSE)

index_young(p1, p0, pb, qb, na.rm = FALSE)

index_fisher(p1, p0, q1, q0, na.rm = FALSE)

index_hlp(p1, p0, q1, q0, na.rm = FALSE)

index_lm(p1, p0, q0, elasticity, na.rm = FALSE)

index_cswd(p1, p0, na.rm = FALSE)

index_cswdb(p1, p0, q1, q0, na.rm = FALSE)

index_bw(p1, p0, na.rm = FALSE)

index_stuval(p1, p0, q1, q0, a, b, na.rm = FALSE)
}

\arguments{
\item{type}{The name of the index. See details.}

\item{p1}{Current-period prices.}

\item{p0}{Base-period prices.}

\item{q1}{Current-period quantities.}

\item{q0}{Base-period quantities.}

\item{pb}{Period-b prices for the Lowe/Young index.}

\item{qb}{Period-b quantities for the Lowe/Young index.}

\item{na.rm}{Should missing values be removed?}

\item{elasticity}{The elasticity of substitution for the Lloyd-Moulton index.}

\item{a, b}{Parameters for the generalized Stuval index.}
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

Weights for the following types of indexes can be calculated.
\itemize{
\item Carli / Jevons / Coggeshall
\item Dutot
\item Laspeyres / Lloyd-Moulton
\item Hybrid Laspeyres (for use in a harmonic mean)
\item Paasche / Palgrave
\item Hybrid Paasche (for use in an arithmetic mean)
\item Tornqvist / Unnamed
\item Drobish
\item Walsh-I (for an arithmetic Walsh index)
\item Walsh-II (for a geometric Walsh index)
\item Marshall-Edgeworth
\item Geary-Khamis
\item Montgomery-Vartia / Vartia-I
\item Sato-Vartia / Vartia-II
\item Lowe
\item Young
}

Naming for the weights follows the CPI/PPI manual first, then Balk (2008) for indexes not listed (or not named) in the CPI/PPI manual. In several cases two or more names correspond to the same weights (e.g., Paasche and Palgrave, or Sato-Vartia and Vartia-II). The explicit calculations are given in the examples.

The weights need not sum to 1, as this normalization isn't always appropriate (i.e., for the Vartia-I weights).
}

\note{
Dealing with missing values is cumbersome when making a price index, and best avoided. As there are different approaches for dealing with missing values in a price index, missing values should be dealt with prior to calculating the index. 

The approach taken here when \code{na.rm = TRUE} is to remove price relatives with missing information, either because of a missing price or a missing weight. Certain properties of an index-number formula may not work as expected with missing values, however, if there is ambiguity about how to remove missing values from the weights (as in, e.g., a Tornqvist or Sato-Vartia index).

When making weights, setting \code{na.rm = TRUE} simply remove missing values when scaling the weights if \code{scale = TRUE}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 
}

\examples{
# Make some data

p0 <- price6[[2]]
p1 <- price6[[3]]
q0 <- quantity6[[2]]
q1 <- quantity6[[3]]
pb <- price6[[1]]
qb <- quantity6[[1]]

# Most of these indexes can be calculated by combining the 
# appropriate weights with the correct type of mean

index_geometric("Laspeyres")(p1, p0, q0)
mean_geometric(p1 / p0, index_weights("Laspeyres")(p0, q0))

# Arithmetic Laspeyres index

mean_arithmetic(p1 / p0, index_weights("Laspeyres")(p0, q0)) 

# Trivial to turn this into a basket-style index

qs <- index_weights("Laspeyres")(p0, q0) / p0
sum(p1 * qs) / sum(p0 * qs)

# Harmonic calculation
mean_harmonic(p1 / p0, index_weights("HybridLaspeyres")(p1, q0)) 

# If only weights are available (no quantity information), then use one of the generalized means

w <- c(0.1, 0.2, 0.4, 0.15, 0.10, 0.05)
mean_geometric(p1 / p0, w)

# Chain an index by price updating the weights

p2 <- price6[[3]]
index_laspeyres(p2, p0, q0)

I1 <- index_laspeyres(p1, p0, q0) 
w_pu <- weights_update(p1 / p0, index_weights("Laspeyres")(p0, q0))
I2 <- mean_arithmetic(p2 / p1, w_pu)
I1 * I2

# Works for other types of indexes, too

index_harmonic("Laspeyres")(p2, p0, q0)

I1 <- index_harmonic("Laspeyres")(p1, p0, q0) 
w_pu <- weights_factor(-1)(p1 / p0, index_weights("Laspeyres")(p0, q0))
I2 <- mean_harmonic(p2 / p1, w_pu)
I1 * I2

# Quote contribution for the Tornqvist index

(con <- weights_g2a(p1 / p0, index_weights("Tornqvist")(p1, p0, q1, q0)) * (p1 / p0 - 1))

sum(con)
index_geometric("Tornqvist")(p1, p0, q1, q0) - 1

# Quote contribution for the Fisher index

wl <- index_weights("Laspeyres")(p0, q0)
wp <- index_weights("HybridPaasche")(p0, q1)
wf <- weights_g2a(c(mean_arithmetic(p1 / p0, wl), mean_arithmetic(p1 / p0, wp)))
(con <- (wf[1] * wl + wf[2] * wp) * (p1 / p0 - 1))

sum(con)
index_fisher(p1, p0, q1, q0) - 1

# NAs get special treatment

p0[6] <- NA

# Drops the last price relative

index_laspeyres(p1, p0, q0, na.rm = TRUE)

# Only drops the last period-0 price

sum(p1 * q0, na.rm = TRUE) / sum(p0 * q0, na.rm = TRUE)

# Explicit calculation for each of the different weights
# Carli/Jevons/Coggeshall

index_weights("Carli")(p1)
rep(1 / length(p0), length(p0))

# Dutot

index_weights("Dutot")(p0)
p0 / sum(p0)

# Laspeyres / Lloyd-Moulton

index_weights("Laspeyres")(p0, q0)
p0 * q0 / sum(p0 * q0)

# Hybrid Laspeyres

index_weights("HybridLaspeyres")(p1, q0)
p1 * q0 / sum(p1 * q0)

# Paasche / Palgrave

index_weights("Paasche")(p1, q1)
p1 * q1 / sum(p1 * q1)

# Hybrid Paasche

index_weights("HybridPaasche")(p0, q1)
p0 * q1 / sum(p0 * q1)

# Tornqvist / Unnamed

index_weights("Tornqvist")(p1, p0, q1, q0)
0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1)

# Drobish

index_weights("Drobish")(p1, p0, q1, q0)
0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p0 * q1 / sum(p0 * q1)

# Walsh-I

index_weights("Walsh1")(p0, q1, q0)
p0 * sqrt(q0 * q1) / sum(p0 * sqrt(q0 * q1))

# Marshall-Edgeworth

index_weights("MarshallEdgeworth")(p0, q1, q0)
p0 * (q0 + q1) / sum(p0 * (q0 + q1))

# Geary-Khamis

index_weights("GearyKhamis")(p0, q1, q0)
p0 / (1 / q0 + 1 / q1) / sum(p0 / (1 / q0 + 1 / q1))

# Montgomery-Vartia / Vartia-I

index_weights("MontgomeryVartia")(p1, p0, q1, q0)
logmean(p0 * q0, p1 * q1) / logmean(sum(p0 * q0), sum(p1 * q1))

# Sato-Vartia / Vartia-II

index_weights("SatoVartia")(p1, p0, q1, q0)
logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)) / 
sum(logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)))

# Walsh-II

index_weights("Walsh2")(p1, p0, q1, q0)
sqrt(p0 * q0 * p1 * q1) / sum(sqrt(p0 * q0 * p1 * q1))

# Lowe

index_weights("Lowe")(p0 = p0, q0 = qb)
p0 * qb / sum(p0 * qb)

# Young

index_weights("Young")(p0 = pb, q0 = qb)
pb * qb / sum(pb * qb)
}