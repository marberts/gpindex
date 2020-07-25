\name{index weights}
\alias{index_weights}

\title{Calculate the weights for a variety of price indexes}

\description{
Calculate the weights for a variety of price indexes using information on prices and quantities.
}

\usage{
index_weights(p1, p0, q1, q0, type, na.rm = FALSE, 
              scale = !is.element(type, c("Vartia1", "MontgomeryVartia")))
}

\arguments{
\item{p1}{Current-period prices.}

\item{p0}{Base-period prices.}

\item{q1}{Current-period quantities.}

\item{q0}{Base-period quantities.}

\item{type}{The name of the index. See details.}

\item{na.rm}{Should missing values be removed when calling \code{\link[gpindex:weights_scale]{weights_scale()}}?}

\item{scale}{Should the weights be scaled to sum to 1?}
}

\value{
A numeric vector, the same length as either \code{p0} or \code{p1}.
}

\details{
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
Dealing with missing values is cumbersome when making weights, and best avoided. As there are different approaches for dealing with missing values in a price index, missing values should be dealt with prior to calculating weights. Setting \code{na.rm = TRUE} simply remove missing values when scaling the weights if \code{scale = TRUE}.
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean.

\code{\link[gpindex]{index}} provides a wrapper for common indexes.

\code{\link[gpindex]{weights_scale}} to scale the weights to sum to 1.
}

\examples{
# Make some data

p0 <- price6[[2]]
p1 <- price6[[3]]
q0 <- quantity6[[2]]
q1 <- quantity6[[3]]
pb <- price6[[1]]
qb <- quantity6[[1]]

# Can be used with the mean_generalized function to make most types of price indexes
# Arithmetic Laspeyres index

mean_arithmetic(p1 / p0, index_weights(p1, p0, q1, q0, type = "Laspeyres")) 

# Trivial to turn this into a basket-style index

qs <- index_weights(p1, p0, q1, q0, type = "Laspeyres") / p0
sum(p1 * qs) / sum(p0 * qs)

# Harmonic calculation
mean_harmonic(p1 / p0, index_weights(p1, p0, q1, q0, type = "HybridLaspeyres")) 

# Explicit calculation for each of the different weights
# Carli/Jevons/Coggeshall

index_weights(p1, p0, q1, q0, type = "Carli")
rep(1 / length(p0), length(p0))

# Dutot

index_weights(p1, p0, q1, q0, type = "Dutot")
p0 / sum(p0)

# Laspeyres / Lloyd-Moulton

index_weights(p1, p0, q1, q0, type = "Laspeyres")
p0 * q0 / sum(p0 * q0)

# Hybrid Laspeyres

index_weights(p1, p0, q1, q0, type = "HybridLaspeyres")
p1 * q0 / sum(p1 * q0)

# Paasche / Palgrave

index_weights(p1, p0, q1, q0, type = "Paasche")
p1 * q1 / sum(p1 * q1)

# Hybrid Paasche

index_weights(p1, p0, q1, q0, type = "HybridPaasche")
p0 * q1 / sum(p0 * q1)

# Tornqvist / Unnamed

index_weights(p1, p0, q1, q0, type = "Tornqvist")
0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1)

# Drobish

index_weights(p1, p0, q1, q0, type = "Drobish")
0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p0 * q1 / sum(p0 * q1)

# Walsh-I

index_weights(p1, p0, q1, q0, type = "Walsh1")
p0 * sqrt(q0 * q1) / sum(p0 * sqrt(q0 * q1))

# Marshall-Edgeworth

index_weights(p1, p0, q1, q0, type = "MarshallEdgeworth")
p0 * (q0 + q1) / sum(p0 * (q0 + q1))

# Geary-Khamis

index_weights(p1, p0, q1, q0, type = "GearyKhamis")
p0 / (1 / q0 + 1 / q1) / sum(p0 / (1 / q0 + 1 / q1))

# Montgomery-Vartia / Vartia-I

index_weights(p1, p0, q1, q0, type = "MontgomeryVartia")
logmean(p0 * q0, p1 * q1) / logmean(sum(p0 * q0), sum(p1 * q1))

# Sato-Vartia / Vartia-II

index_weights(p1, p0, q1, q0, type = "SatoVartia")
logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)) / 
sum(logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)))

# Walsh-II

index_weights(p1, p0, q1, q0, type = "Walsh2")
sqrt(p0 * q0 * p1 * q1) / sum(sqrt(p0 * q0 * p1 * q1))

# Lowe

index_weights(p0 = p0, q0 = qb, type = "Lowe")
p0 * qb / sum(p0 * qb)

# Young

index_weights(p0 = pb, q0 = qb, type = "Young")
pb * qb / sum(pb * qb)
}