\name{price_index}
\alias{price_index}
\alias{arithmetic_index}
\alias{geometric_index}
\alias{harmonic_index}
\alias{laspeyres_index}
\alias{paasche_index}
\alias{jevons_index}
\alias{lowe_index}
\alias{young_index}
\alias{fisher_index}
\alias{hlp_index}
\alias{lm_index}
\alias{arithmetic_agmean_index}
\alias{geometric_agmean_index}
\alias{cswd_index}
\alias{cswdb_index}
\alias{bw_index}
\alias{stuval_index}
\alias{index_weights}

\title{Price indexes}

\description{
Calculate a variety of price indexes using information on prices and quantities.
}

\usage{
arithmetic_index(type)

geometric_index(type)

harmonic_index(type)

laspeyres_index(p1, p0, q0, na.rm = FALSE)

paasche_index(p1, p0, q1, na.rm = FALSE)

jevons_index(p1, p0, na.rm = FALSE)

lowe_index(p1, p0, qb, na.rm = FALSE)

young_index(p1, p0, pb, qb, na.rm = FALSE)

fisher_index(p1, p0, q1, q0, na.rm = FALSE)

hlp_index(p1, p0, q1, q0, na.rm = FALSE)

lm_index(p1, p0, q0, elasticity, na.rm = FALSE)

arithmetic_agmean_index(p1, p0, q0, elasticity, na.rm = FALSE)

geometric_agmean_index(p1, p0, q0, elasticity, na.rm = FALSE)

cswd_index(p1, p0, na.rm = FALSE)

cswdb_index(p1, p0, q1, q0, na.rm = FALSE)

bw_index(p1, p0, na.rm = FALSE)

stuval_index(a, b)

index_weights(type)
}

\arguments{
\item{type}{The name of the index. See details for the possible types of indexes.}

\item{p1}{Current-period prices.}

\item{p0}{Base-period prices.}

\item{q1}{Current-period quantities.}

\item{q0}{Base-period quantities.}

\item{pb}{Period-b prices for the Lowe/Young index.}

\item{qb}{Period-b quantities for the Lowe/Young index.}

\item{na.rm}{Should missing values be removed? By default missing values for prices or quantities return a missing value.}

\item{elasticity}{The elasticity of substitution for the Lloyd-Moulton and AG mean indexes.}

\item{a, b}{Parameters for the generalized Stuval index.}
}

\details{
The \code{arithmetic_index()}, \code{geometric_index()}, and \code{harmonic_index()} functions return a function to calculate a given type of arithmetic, geometric, and harmonic index. Together, these functions produce functions to calculate the following indexes.
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
\item Harmonic Young
}

Along with the \code{lm_index()} function to calculate the Lloyd-Moulton index, these are just convenient wrappers for \code{\link[=generalized_mean]{generalized_mean()}} and \code{index_weights()}. 

The Laspeyres, Paasche, Jevons, Lowe, and Young indexes are among the most common price indexes, and so they get their own functions. The \code{laspeyres_index()}, \code{lowe_index()}, and \code{young_index()} functions correspond to setting the appropriate \code{type} in \code{arithmetic_index()}; \code{paasche_index()} and \code{jevons_index()} instead come from the \code{harmonic_index()} and \code{geometric_index()} functions.

In addition to these generalized-mean indexes, there are also functions for calculating a variety of non-generalized-mean indexes. The Fisher index is the geometric mean of the arithmetic Laspeyres and Paasche indexes; the Harmonic Laspeyres Paasche index is the harmonic analog of the Fisher index. The Carruthers-Sellwood-Ward-Dalen and Carruthers-Sellwood-Ward-Dalen-Balk indexes are sample analogs of the Fisher index; the Balk-Walsh index is the sample analog of the Walsh index. The AG mean index is the arithmetic or geometric mean of the geometric and arithmetic Laspeyres indexes, weighted by the elasticity of substitution. The \code{stuval_index()} function returns a function to calculate a Stuval index of the given parameters.

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

Naming for the indexes and weights generally follows the CPI manual and Balk (2008). In several cases two or more names correspond to the same weights (e.g., Paasche and Palgrave, or Sato-Vartia and Vartia-II). The calculations are given in the examples.
}

\value{
\code{arithmetic_index()}, \code{geometric_index()}, \code{harmonic_index()}, \code{stuval_index()}, and \code{index_weights()} each return a function to compute the relevant price indexes; the others return a numeric value giving the change in price between the base period and current period.
}

\note{
Dealing with missing values is cumbersome when making a price index, and best avoided. As there are different approaches for dealing with missing values in a price index, missing values should be dealt with prior to calculating the index. 

The approach taken when \code{na.rm = TRUE} removes price relatives with missing information, either because of a missing price or a missing weight. Certain properties of an index-number formula may not work as expected with missing values, however, if there is ambiguity about how to remove missing values from the weights (as in, e.g., a Tornqvist or Sato-Vartia index).
}

\references{
Balk, B. M. (2008). \emph{Price and Quantity Index Numbers}. Cambridge University Press.

ILO, IMF, OECD, Eurostat, UN, and World Bank. (2020). \emph{Consumer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link{generalized_mean}} for the generalized mean. 

\code{\link{contributions}} for calculating quote contributions. 

\code{\link{update_weights}} for price-updating weights.

\code{\link{quantity_index}} to remap the arguments in these functions for a quantity index.
}

\examples{
p0 <- price6[[2]]
p1 <- price6[[3]]
q0 <- quantity6[[2]]
q1 <- quantity6[[3]]
pb <- price6[[1]]
qb <- quantity6[[1]]

#---- Calculating price indexes ----

# Most indexes can be calculated by combining the appropriate 
# weights with the correct type of mean

geometric_index("Laspeyres")(p1, p0, q0)
geometric_mean(p1 / p0, index_weights("Laspeyres")(p0, q0))

# Arithmetic Laspeyres index

laspeyres_index(p1, p0, q0)
arithmetic_mean(p1 / p0, index_weights("Laspeyres")(p0, q0)) 

# Harmonic calculation for the arithmetic Laspeyres

harmonic_mean(p1 / p0, index_weights("HybridLaspeyres")(p1, q0))

# Same as transmuting the weights

all.equal(
  scale_weights(index_weights("HybridLaspeyres")(p1, q0)),
  scale_weights(transmute_weights(1, -1)(p1 / p0, index_weights("Laspeyres")(p0, q0)))
)

# Unlike its arithmetic counterpart, the geometric Laspeyres 
# can increase when base-period prices increase if some of these 
# prices are small

p0_small <- replace(p0, 1, p0[1] / 5)
p0_dx <- replace(p0_small, 1, p0_small[1] + 0.01)
geometric_index("Laspeyres")(p1, p0_small, q0) < 
    geometric_index("Laspeyres")(p1, p0_dx, q0)

#---- Price updating the weights in a price index ----

# Chain an index by price updating the weights

p2 <- price6[[4]]
laspeyres_index(p2, p0, q0)

I1 <- laspeyres_index(p1, p0, q0) 
w_pu <- update_weights(p1 / p0, index_weights("Laspeyres")(p0, q0))
I2 <- arithmetic_mean(p2 / p1, w_pu)
I1 * I2

# Works for other types of indexes, too

harmonic_index("Laspeyres")(p2, p0, q0)

I1 <- harmonic_index("Laspeyres")(p1, p0, q0) 
w_pu <- factor_weights(-1)(p1 / p0, index_weights("Laspeyres")(p0, q0))
I2 <- harmonic_mean(p2 / p1, w_pu)
I1 * I2

#---- Quote contributions ----

# Quote contributions for the Tornqvist index

w <- index_weights("Tornqvist")(p1, p0, q1, q0)
(con <- geometric_contributions(p1 / p0, w))

all.equal(sum(con), geometric_index("Tornqvist")(p1, p0, q1, q0) - 1)

#---- Missing values ----

# NAs get special treatment

p_na <- replace(p0, 6, NA)

# Drops the last price relative

laspeyres_index(p1, p_na, q0, na.rm = TRUE)

# Only drops the last period-0 price

sum(p1 * q0, na.rm = TRUE) / sum(p_na * q0, na.rm = TRUE)

#---- Making the weights for different indexes ----

# Explicit calculation for each of the different weights
# Carli/Jevons/Coggeshall

all.equal(index_weights("Carli")(p1), rep(1, length(p0)))

# Dutot

all.equal(index_weights("Dutot")(p0), p0)

# Laspeyres / Lloyd-Moulton

all.equal(index_weights("Laspeyres")(p0, q0), p0 * q0)

# Hybrid Laspeyres

all.equal(index_weights("HybridLaspeyres")(p1, q0), p1 * q0)

# Paasche / Palgrave

all.equal(index_weights("Paasche")(p1, q1), p1 * q1)

# Hybrid Paasche

all.equal(index_weights("HybridPaasche")(p0, q1), p0 * q1)

# Tornqvist / Unnamed

all.equal(index_weights("Tornqvist")(p1, p0, q1, q0),
          0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p1 * q1 / sum(p1 * q1))

# Drobish

all.equal(index_weights("Drobish")(p1, p0, q1, q0),
          0.5 * p0 * q0 / sum(p0 * q0) + 0.5 * p0 * q1 / sum(p0 * q1))

# Walsh-I

all.equal(index_weights("Walsh1")(p0, q1, q0),
          p0 * sqrt(q0 * q1))

# Marshall-Edgeworth

all.equal(index_weights("MarshallEdgeworth")(p0, q1, q0),
          p0 * (q0 + q1))

# Geary-Khamis

all.equal(index_weights("GearyKhamis")(p0, q1, q0),
          p0 / (1 / q0 + 1 / q1))

# Montgomery-Vartia / Vartia-I

all.equal(index_weights("MontgomeryVartia")(p1, p0, q1, q0),
          logmean(p0 * q0, p1 * q1) / logmean(sum(p0 * q0), sum(p1 * q1)))

# Sato-Vartia / Vartia-II

all.equal(index_weights("SatoVartia")(p1, p0, q1, q0),
          logmean(p0 * q0 / sum(p0 * q0), p1 * q1 / sum(p1 * q1)))

# Walsh-II

all.equal(index_weights("Walsh2")(p1, p0, q1, q0),
          sqrt(p0 * q0 * p1 * q1))

# Lowe

all.equal(index_weights("Lowe")(p0, qb), p0 * qb)

# Young

all.equal(index_weights("Young")(pb, qb), pb * qb)
}