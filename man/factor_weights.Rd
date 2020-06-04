\name{factor weights}
\alias{weights_factor}

\title{Factor the weights in a generalized mean}

\description{
Calculate the weights to turn the generalized mean of a product into the product of generalized means.
}

\usage{
weights_factor(x, w, r, na.rm = FALSE, scale = TRUE)
}

\arguments{
\item{x}{A numeric vector.}

\item{w}{An optional vector of numeric weights, the same length as \code{x}.}

\item{r}{A number giving the exponent of the generalized mean.}

\item{na.rm}{Should missing values be removed when calling \code{\link[gpindex:weights_scale]{weights_scale()}}?}

\item{scale}{Should the weights be scaled to sum to 1?}
}

\value{
A numeric vector, the same length as \code{x}.
}

\details{
Returns a vector of weights \code{v} such that 

\code{mean_generalized(x * y, w, r) == mean_generalized(x, w, r) * mean_generalized(y, v, r)}. 

This generalizes the result in section C.5 of Chapter 9 of the PPI Manual for the Young index.

As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforced here, but the results may not make sense otherwise.
}

\references{
ILO, IMF, OECD, Eurostat, UN, and World Bank. (2004). \emph{Producer Price Index Manual: Theory and Practice}. International Monetary Fund.
}

\seealso{
\code{\link[gpindex]{mean_generalized}} for the generalized mean.

\code{\link[gpindex]{weights_scale}} to scale the weights to sum to 1.
}

\examples{
# Make some data

x <- 1:10
y <- 11:20
w <- runif(10)

# Calculate the harmonic mean

mean_harmonic(x * y, w)

# The same as

mean_harmonic(x, w) * mean_harmonic(y, weights_factor(x, w, -1))

# The common case of an arithmetic mean

mean_arithmetic(x * y, w)

mean_arithmetic(x, w) * mean_arithmetic(y, weights_factor(x, w, 1))

}
