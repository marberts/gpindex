\name{price indexes}
\alias{index}
\alias{index_arithmetic}
\alias{index_geometric}
\alias{index_harmonic}
\alias{index_laspeyres}
\alias{index_paasche}
\alias{index_jevons}
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

index_laspeyres(p1, p0, q0, na.rm = FALSE)

index_paasche(p1, p0, q1, na.rm = FALSE)

index_jevons(p1, p0, na.rm = FALSE)

index_lowe(p1, p0, qb, na.rm = FALSE)

index_young(p1, p0, pb, qb, na.rm = FALSE)

index_fisher(p1, p0, q1, q0, na.rm = FALSE)

index_hlp(p1, p0, q1, q0, na.rm = FALSE)

index_lm(p1, p0, q0, elasticity, na.rm = FALSE)

index_cswd(p1, p0, na.rm = FALSE)

index_cswdb(p1, p0, q1, q0, na.rm = FALSE)

index_bw(p1, p0, na.rm = FALSE)

index_stuval(a, b)

index_weights(type)
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
\code{index_arithmetic()}, \code{index_geometric()}, \code{index_harmonic()}, \code{index_stuval()}, and \code{index_weights()} each return a function; the others return a numeric value.
}

\details{
The \code{arithmetic_mean()}, \code{geometric_mean()}, and \code{harmonic_mean()} functions return a function to calculate a given type of arithmetic, geometric, and harmonic index. Together, these functions produce functions to calculate the following indexes.
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
\item Geometric Young
\item Tornqvist
\item Montgomery-Vartia / Vartia-I
\item Sato-Vartia / Vartia-II
\item Walsh-II (geometric Walsh)
\item \bold{Harmonic indexes}
\item Coggeshall (equally weighted harmonic index)
\item Paasche
\item Harmonic Laspeyres
}

Along with the \code{index_lm()} function to calculate the Lloyd-Moulton index, these are just convenient wrappers for \code{\link[gpindex:mean_generalized]{mean_generalized()}}. 

The Laspeyres, Paasche, Jevons, Lowe, and Young indexes are among the most common price indexes. The \code{index_laspeyres()}, \code{index_lowe()}, and \code{index_young()} functions correspond to setting the appropriate \code{type} in \code{index_arithmetic()}; \code{index_paasche()} and \code{index_jevons()} instead come from the \code{index_harmonic()} and \code{index_geometric()} functions.

In addition to these generalized indexes, there are also functions for calculating a variety of non-generalized indexes. The Fisher index is the geometric mean of the arithmetic Laspeyres and Paasche indexes; the Harmonic Laspeyres Paasche index is the harmonic analog of the Fisher index. The Carruthers-Sellwood-Ward-Dalen and Carruthers-Sellwood-Ward-Dalen-Balk indexes are sample analogs of the Fisher index; the Balk-Walsh index is the sample analog of the Walsh index. The \code{index_stuval()} function returns a function to calculate a Stuval index of the given parameters.

The \code{index_weights()} function returns a function to calculate weights for a variety of price indexes. Weights for the following types of indexes can be calculated.
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

The weights need not sum to 1, as this normalization isn't always appropriate (i.e., for the Vartia-I weights).

Naming for the indexes and weights generally follows the CPI/PPI manual first, then Balk (2008) for indexes not listed (or not named) in the CPI/PPI manual. In several cases two or more names correspond to the same weights (e.g., Paasche and Palgrave, or Sato-Vartia and Vartia-II). The calculations are given in the examples.
}

\note{
Dealing with missing values is cumbersome when making a price index, and best avoided. As there are different approaches for dealing with missing values in a price index, missing values should be dealt with prior to calculating the index. 

The approach taken here when \code{na.rm = TRUE} is to remove price relatives with missing information, either because of a missing price or a missing weight. Certain properties of an index-number formula may not work as expected with missing values, however, if there is ambiguity about how to remove missing values from the weights (as in, e.g., a Tornqvist or Sato-Vartia index).
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean. 

\code{\link[gpindex]{contributions}} for calculating quote contributions. 
}

\examples{
# Make some data

p0 <- price6[[2]]
p1 <- price6[[3]]
q0 <- quantity6[[2]]
q1 <- quantity6[[3]]
pb <- price6[[1]]
qb <- quantity6[[1]]

# Most indexes can be calculated by combining the appropriate weights with
# the correct type of mean

index_geometric("Laspeyres")(p1, p0, q0)
mean_geometric(p1 / p0, index_weights("Laspeyres")(p0, q0))

# Arithmetic Laspeyres index

index_laspeyres(p1, p0, q0)
mean_arithmetic(p1 / p0, index_weights("Laspeyres")(p0, q0)) 

# Trivial to turn weights for an arithmetic index 
# into a basket-style index

qs <- index_weights("Laspeyres")(p0, q0) / p0
sum(p1 * qs) / sum(p0 * qs)

# Harmonic calculation for the arithmetic Laspeyres

mean_harmonic(p1 / p0, index_weights("HybridLaspeyres")(p1, q0))

# Unlike its arithmetic counterpart, the geometric Laspeyres can increase
# when base-period prices increase if some of these prices are small

p0_small <- replace(p0, 1, p0[1] / 5)
p0_dx <- replace(p0_small, 1, p0_small[1] + 0.01)
index_geometric("Laspeyres")(p1, p0_small, q0) < 
    index_geometric("Laspeyres")(p1, p0_dx, q0)

#--------------------

# Chain an index by price updating the weights

p2 <- price6[[4]]
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

#--------------------

# Quote contributions for the Tornqvist index

w <- index_weights("Tornqvist")(p1, p0, q1, q0)
(con <- contributions_geometric(p1 / p0, w))

sum(con)
index_geometric("Tornqvist")(p1, p0, q1, q0) - 1

# Quote contributions for the Fisher index

wl <- weights_scale(index_weights("Laspeyres")(p0, q0))
wp <- weights_scale(index_weights("HybridPaasche")(p0, q1))
wf <- weights_transmute(0, 1)(c(mean_arithmetic(p1 / p0, wl), 
                                mean_arithmetic(p1 / p0, wp)))
wf <- weights_scale(wf)
(con <- (wf[1] * wl + wf[2] * wp) * (p1 / p0 - 1))

sum(con)
index_fisher(p1, p0, q1, q0) - 1

# The same as the decomposition in section 4.2.2 of Balk (2008)

Qf <- index_fisher(q1, q0, p1, p0)
Ql <- index_laspeyres(q1, q0, p0)
con2 <- (Qf / (Qf + Ql) * wl + Ql / (Qf + Ql) * wp) * (p1 / p0 - 1)
all.equal(con, con2)

#--------------------

# NAs get special treatment

p_na <- replace(p0, 6, NA)

# Drops the last price relative

index_laspeyres(p1, p_na, q0, na.rm = TRUE)

# Only drops the last period-0 price

sum(p1 * q0, na.rm = TRUE) / sum(p_na * q0, na.rm = TRUE)

#--------------------

# Explicit calculation for each of the different weights
# Carli/Jevons/Coggeshall

index_weights("Carli")(p1)
rep(1, length(p0))

# Dutot

index_weights("Dutot")(p0)
p0

# Laspeyres / Lloyd-Moulton

index_weights("Laspeyres")(p0, q0)
p0 * q0

# Hybrid Laspeyres

index_weights("HybridLaspeyres")(p1, q0)
p1 * q0

# Paasche / Palgrave

index_weights("Paasche")(p1, q1)
p1 * q1

# Hybrid Paasche

index_weights("HybridPaasche")(p0, q1)
p0 * q1

# Tornqvist / Unnamed

index_weights("Tornqvist")(p1, p0, q1, q0)
0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1)

# Drobish

index_weights("Drobish")(p1, p0, q1, q0)
0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p0 * q1 / sum(p0 * q1)

# Walsh-I

index_weights("Walsh1")(p0, q1, q0)
p0 * sqrt(q0 * q1)

# Marshall-Edgeworth

index_weights("MarshallEdgeworth")(p0, q1, q0)
p0 * (q0 + q1)

# Geary-Khamis

index_weights("GearyKhamis")(p0, q1, q0)
p0 / (1 / q0 + 1 / q1)

# Montgomery-Vartia / Vartia-I

index_weights("MontgomeryVartia")(p1, p0, q1, q0)
logmean(p0 * q0, p1 * q1) / logmean(sum(p0 * q0), sum(p1 * q1))

# Sato-Vartia / Vartia-II

index_weights("SatoVartia")(p1, p0, q1, q0)
logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1))

# Walsh-II

index_weights("Walsh2")(p1, p0, q1, q0)
sqrt(p0 * q0 * p1 * q1)

# Lowe

index_weights("Lowe")(p0, qb)
p0 * qb

# Young

index_weights("Young")(pb, qb)
pb * qb
}