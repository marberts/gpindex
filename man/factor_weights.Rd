\name{factor weights}
\alias{factor_weights}
\alias{price_update}

\title{Factor the weights in a generalized mean}

\description{
Calculate the weights to turn the generalized mean of a product into the product of generalized means.
}

\usage{
factor_weights(x, w, r)

price_update(x, w)
}

\arguments{
\item{x}{A numeric vector.}

\item{w}{An optional vector of numeric weights, the same length as \code{x}.}

\item{r}{A number giving the exponent of the generalized mean. See details.}
}

\value{
A numeric vector, the same length as \code{w}.
}

\details{
Returns a vector of weights \code{v} such that \code{generalized_mean(x * y, w, r) = generalized_mean(x, w, r) * generalized_mean(y, v, r)}. This generalizes the result in section C.5 of Chapter 9 of the PPI Manual for the Young index.

Factoring the weights for an average often gets called price-updating the weights for an arithmetic price index; \code{price_update(x, w)} is a simple wrapper for \code{price_update(x, w, 1)}.

As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforeced here, but the results may not make sense otherwise.
}

\references{
ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). Producer Price Index Manual: Theory and Practice. International Monetary Fund.
}

\seealso{
See \code{\link[gpindex]{generalized_mean}} for the generalized mean.
}

\examples{
# Make some data

x <- 1:10
y <- 11:20
w <- runif(10)

# Calculate the harmonic mean

harmonic_mean(x * y, w)

# The same as

harmonic_mean(x, w) * harmonic_mean(y, factor_weights(x, w, -1))

# price_update is for the common case of an arithmetic mean

arithmetic_mean(x * y, w)

arithmetic_mean(x, w) * arithmetic_mean(y, price_update(x, w))

}
