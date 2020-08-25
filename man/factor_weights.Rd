\name{factor weights}
\alias{weights_factor}
\alias{weights_update}

\title{Factor the weights in a generalized mean}

\description{
Calculate the weights to turn the generalized mean of a product into the product of generalized means.
}

\usage{
weights_factor(r)

weights_update(x, w = rep(1, length(x)), na.rm = FALSE, scale = TRUE)
}

\arguments{
\item{r}{A number giving the exponent of the generalized mean.}

\item{x}{A numeric vector.}

\item{w}{A numeric vector of weights, the same length as \code{x}. The default is to equally weight each element of \code{x}.}

\item{na.rm}{Should missing values be removed when calling \code{\link[gpindex:weights_scale]{weights_scale()}}?}

\item{scale}{Should the weights be scaled to sum to 1?}
}

\value{
\code{weights_factor()} returns a function: \code{function(x, w, na.rm, scale)}. \code{weights_update()} returns a numeric vector the same length as \code{x}.
}

\details{
The function \code{weights_factor()} returns a function to compute weights \code{v(x, w)} such that 

\code{mean_generalized(r)(x * y, w) == mean_generalized(r)(x, w) * mean_generalized(r)(y, v(x, w))}. 

This generalizes the result in section C.5 of Chapter 9 of the PPI Manual for the Young index. Factoring weights with \code{r = 1} sometimes gets called price-updating weights; \code{weights_update()} simply calls \code{weights_factor()} with \code{r = 1}.

As a matter of definition, both \code{x} and \code{w} should be strictly positive. This is not enforced here, but the results may not make sense in cases where the generalized mean is not well defined.
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

mean_harmonic(x, w) * mean_harmonic(y, weights_factor(-1)(x, w))

# The common case of an arithmetic mean

mean_arithmetic(x * y, w)

mean_arithmetic(x, w) * mean_arithmetic(y, weights_update(x, w))

# In cases where x and y have the same order, Chebyshev's inequality
# implies that the chained calculation is too small

mean_arithmetic(x * y, w) > mean_arithmetic(x, w) * mean_arithmetic(y, w)
}